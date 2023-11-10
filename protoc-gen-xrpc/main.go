// Package gen -----------------------------
// @file      : test.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/23 18:51
// -------------------------------------------
package main

import (
	"fmt"
	"google.golang.org/protobuf/compiler/protogen"
	"os"
	"path"
	"regexp"
	"strings"
	"text/template"
)

var erlTargetPath = "./erlang2/libs/xrpc/src/gen"
var golangTargetPath = "./golang/xrpc"

func main() {

	protogen.Options{}.Run(func(gen *protogen.Plugin) error {
		for _, file := range gen.Files {
			if !file.Generate {
				continue
			}
			generateFile(file)
		}
		return nil
	})
}

func generateFile(file *protogen.File) {
	if len(file.Services) == 0 {
		return
	}

	var services []*ServiceDesc
	for _, svr := range file.Services {
		var methods []*MethodDesc
		for _, method := range svr.Methods {
			methods = append(methods, &MethodDesc{
				Name:       method.GoName,
				ErlName:    convertToUnderLineCase(method.GoName),
				InputType:  convertToCamelCase(string(method.Input.Desc.FullName())),
				OutputType: convertToCamelCase(string(method.Output.Desc.FullName())),
			})
		}
		isActor, err := regexp.MatchString("Actor", string(svr.Comments.Leading))
		if err != nil {
			return
		}
		services = append(services, &ServiceDesc{
			Name:    svr.GoName,
			ErlName: convertToUnderLineCase(svr.GoName),
			Methods: methods,
			IsActor: isActor,
		})
	}

	fileSp := strings.Split(*file.Proto.Name, ".")
	erlFileName := fileSp[0] + "_service"
	pd := &ProtoDesc{
		FileName:       file.Desc.Path(),
		Package:        string(file.GoPackageName),
		Services:       services,
		ErlPackageName: erlFileName,
	}
	// generate golang code
	goFileName := fmt.Sprintf("gen_%s_service.go", fileSp[0])
	goFileName = path.Join(golangTargetPath, goFileName)

	err := renderFile(goFileName, pd, "xrpc-go.tmpl")
	if err != nil {
		fmt.Printf("RenderFile Error = %s", err)
		return
	}
	var nameindex []*NameIndex
	for _, svr := range pd.Services {
		if svr.IsActor {
			continue
		}
		// generate erlang code
		erlFileName = path.Join(erlTargetPath, svr.ErlName+".erl")
		err = renderFile(erlFileName, svr, "xrpc-erl.tmpl")
		if err != nil {
			fmt.Printf("RenderFile Error = %s", err)
			return
		}
		for _, md := range svr.Methods {
			nameindex = append(nameindex, &NameIndex{
				GoFuncName:  md.Name,
				ErlFuncName: md.ErlName,
			})
		}
	}
	// generate erlang name index
	err = renderFile(path.Join(erlTargetPath, "func_name_index.erl"), nameindex, "erl_name_index.tmpl")
	if err != nil {
		fmt.Printf("RenderFile Error = %s", err)
		return
	}
}

func renderFile(fileName string, data any, tpName string) error {
	tp, err := template.ParseFiles("./protoc-gen-xrpc/template/" + tpName)
	if err != nil {
		return err
	}
	fs, err := os.Create(fileName)
	if err != nil {
		return err
	}
	defer fs.Close()
	return tp.Execute(fs, data)
}

func convertToCamelCase(s string) string {
	sp := strings.Split(s, ".")
	words := strings.Split(sp[1], "_")
	for i := 0; i < len(words); i++ {
		words[i] = strings.Title(words[i])
	}
	return strings.Join([]string{sp[0], strings.Join(words, "")}, ".")
}

func convertToUnderLineCase(s string) string {
	lastUpKey := -1
	var sp []string
	for i := 0; i < len(s); i++ {
		if s[i] >= 'A' && s[i] <= 'Z' {
			if lastUpKey != -1 {
				sp = append(sp, strings.ToLower(s[lastUpKey:i]))
			}
			lastUpKey = i
		}
	}
	sp = append(sp, strings.ToLower(s[lastUpKey:]))
	return strings.Join(sp, "_")
}
