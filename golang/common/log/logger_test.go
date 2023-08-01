//logger和sugar logger性能测试
//对简单数据日志和复杂数据日志分别进行了测试，结论如下
//-简单数据日志：logger和sugar logger性能相差不大
//-复杂数据日志：数据结构越复杂，logger比sugar logger性能提升越明显
package log

import (
	"testing"
	"time"

	"go.uber.org/multierr"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
)

var (
	_tenInts    = []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 0}
	_tenStrings = []string{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j"}

	_oneUser = user{
		Name:      "william",
		Email:     "william@lilith.com",
		CreatedAt: time.Date(2021, 1, 1, 12, 0, 0, 0, time.UTC),
	}
	_tenUsers = users{
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
		_oneUser,
	}
)

type user struct {
	Name      string    `json:"name"`
	Email     string    `json:"email"`
	CreatedAt time.Time `json:"created_at"`
}

type users []user

func (u user) MarshalLogObject(enc zapcore.ObjectEncoder) error {
	enc.AddString("name", u.Name)
	enc.AddString("email", u.Email)
	enc.AddInt64("createdAt", u.CreatedAt.UnixNano())
	return nil
}

func (uu users) MarshalLogArray(arr zapcore.ArrayEncoder) error {
	var err error
	for i := range uu {
		err = multierr.Append(err, arr.AppendObject(uu[i]))
	}
	return err
}

func BenchmarkInfo(b *testing.B) {
	InitLog("./", "test", "info")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Info("",
			zap.String("string", "hello"),
			zap.String("string", "world"),
			zap.Int8("int", 1),
			zap.Ints("ints", _tenInts),
			zap.Strings("strings", _tenStrings),
			zap.Object("user", _oneUser),
			zap.Array("users", _tenUsers),
		)
	}
}

func BenchmarkSugarInfof(b *testing.B) {
	InitLog("./", "sugar_test", "info")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Infof("%v %v %v %v %v %v %v", fakeFmtArgs()...)
	}
}

func BenchmarkInfoSimple(b *testing.B) {
	InitLog("./", "test", "info")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Info("",
			zap.String("string", "hello"),
			zap.String("string", "world"),
			zap.Int8("int", 1),
			zap.Int8("int", 2),
			zap.Int8("int", 3),
		)
	}
}

func BenchmarkSugarInfofSimple(b *testing.B) {
	InitLog("./", "sugar_test", "info")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Infof("%v %v %v %v %v", fakeFmtArgsSimple()...)
	}
}

func fakeFmtArgs() []interface{} {
	// Need to keep this a function instead of a package-global var so that we
	// pay the cast-to-interface{} penalty on each call.
	return []interface{}{
		"hello",
		"world",
		1,
		_tenInts,
		_tenStrings,
		_oneUser,
		_tenUsers,
	}
}

func fakeFmtArgsSimple() []interface{} {
	// Need to keep this a function instead of a package-global var so that we
	// pay the cast-to-interface{} penalty on each call.
	return []interface{}{
		"hello",
		"world",
		1,
		2,
		3,
	}
}
