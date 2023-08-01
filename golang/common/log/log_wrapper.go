package log

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path"
	"runtime"
	"sync"

	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

const (
	DefaultLogDir = "logs"
)

type prefixContent struct {
	prefix string
	fmts   string
	fields []interface{}
	pair   sync.Map
}

type logStringer struct {
	goID int64
	fmts string
	args []interface{}
}

var additionalInfo prefixContent

func newLogstr(fmts string, args ...interface{}) *logStringer {
	return &logStringer{
		fmts: fmts,
		args: args,
	}
}

func (v *logStringer) String() string {
	return fmt.Sprintf(v.fmts, v.args...)
}

// InitLog 初始化日志
func InitLog(path, filename, level string) error {
	var err error
	// 创建日志路径
	if _, err = createPath(path); err != nil {
		return err
	}

	// 设置日志级别
	var lv zapcore.Level
	if err = lv.Set(level); err != nil {
		return err
	}
	defaultLogger.atomicLevel.SetLevel(lv)

	additionalInfo = prefixContent{"", "", []interface{}{}, sync.Map{}}

	if runtime.GOOS != "windows" {
		return initZapLoggerWithoutConsole(path, filename, lv)
	}
	return initZapLogger(path, filename, lv)
}

func HasPrefix(fmt string) bool {
	_, ok := additionalInfo.pair.Load(fmt)
	return ok
}
func SetPrefixContent(newfmt string, arg interface{}) {
	if index, ok := additionalInfo.pair.Load(newfmt); ok {
		additionalInfo.fields[index.(int)] = arg
	} else {
		additionalInfo.fmts += newfmt
		additionalInfo.fields = append(additionalInfo.fields, arg)
		additionalInfo.pair.Store(newfmt, len(additionalInfo.fields)-1)
	}
	additionalInfo.prefix = fmt.Sprintf(additionalInfo.fmts, additionalInfo.fields...)
}

// 1.Infof、Errorf等支持printf style的函数使用sprintf（底层使用反射），复杂结构日志打印性能比Info、Error差
// 2.Info、Error适用于一些热点函数里的复杂结构打印
// 3.普通业务使用f系列函数即可
// 可以参考logger_test.go文件里的测试结论

func Info(msg string, fields ...zap.Field) {
	defaultLogger.logger.Info(msg, fields...)
}

func Error(msg string, fields ...zap.Field) {
	defaultLogger.logger.Error(msg, fields...)
}

func Debugf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Debugf(additionalInfo.prefix+fmts, args...)
}

func Infof(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Infof(additionalInfo.prefix+fmts, args...)
}

func Warnf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Warnf(additionalInfo.prefix+fmts, args...)
}

func Errorf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Errorf(additionalInfo.prefix+fmts, args...)

}

func DPanicf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.DPanicf(additionalInfo.prefix+fmts, args...)
}

func Panicf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Panicf(additionalInfo.prefix+fmts, args...)
}

func Fatalf(fmts string, args ...interface{}) {
	defaultLogger.sugaredLogger.Fatalf(additionalInfo.prefix+fmts, args...)
}

func SkipLogger(skip int) *zap.SugaredLogger {
	// https://pkg.go.dev/go.uber.org/zap#SugaredLogger.WithOptions
	// WithOptions 克隆当前的 SugaredLogger，应用提供的 Option，然后返回结果，是并发安全的。
	return defaultLogger.sugaredLogger.WithOptions(zap.AddCallerSkip(skip))
}

// createPath 创建日志路径
func createPath(logPath string) (string, error) {
	logPath = path.Clean(logPath)
	if logPath == "" {
		return "", errors.New("log path is empty")
	}

	if !fileExists(logPath) {
		if err := os.MkdirAll(logPath, os.ModePerm); err != nil {
			return "", fmt.Errorf("check log path err:%w", err)
		}
	}

	return logPath, nil
}

// fileExists 检查文件是否存在
func fileExists(path string) bool {
	_, err := os.Stat(path)
	return err == nil || errors.Is(err, fs.ErrExist)
}
