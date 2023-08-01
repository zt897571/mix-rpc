package log

import (
	"github.com/natefinch/lumberjack"
	"go.uber.org/zap"
	"go.uber.org/zap/zapcore"
	"os"
	"path/filepath"
)

var defaultLogger = newLogger()

type DebugEnabled struct{}
type InfoEnabled struct{}
type ErrorEnabled struct{}

type zapLogger struct {
	logger        *zap.Logger
	sugaredLogger *zap.SugaredLogger
	atomicLevel   zap.AtomicLevel
}

func newLogger() *zapLogger {
	return &zapLogger{
		atomicLevel: zap.NewAtomicLevel(),
	}
}

func (d DebugEnabled) Enabled(level zapcore.Level) bool {
	return level >= zapcore.DebugLevel
}

func (l InfoEnabled) Enabled(level zapcore.Level) bool {
	return level >= zapcore.InfoLevel
}

func (e ErrorEnabled) Enabled(level zapcore.Level) bool {
	return level >= zapcore.ErrorLevel
}

func init() {
	consoleCore, err := getConsoleCore(defaultLogger.atomicLevel)
	if err == nil {
		defaultLogger.logger = zap.New(zapcore.NewTee(consoleCore), zap.AddCaller(), zap.AddCallerSkip(1))
		defaultLogger.sugaredLogger = defaultLogger.logger.Sugar()
	} else {
		panic(err)
	}
}

func initZapLogger(pathName string, logName string, level zapcore.Level) error {
	consoleCore, err := getConsoleCore(defaultLogger.atomicLevel)
	if err != nil {
		return err
	}

	infoCore, err := getInfoCore(pathName, logName)
	if err != nil {
		return err
	}

	errorCore, err := getErrorCore(pathName, logName)
	if err != nil {
		return err
	}

	if level == zapcore.DebugLevel {
		debugCore, err := getDebugCore(pathName, logName)
		if err != nil {
			return err
		}
		defaultLogger.logger = zap.New(zapcore.NewTee(consoleCore, debugCore, infoCore, errorCore),
			zap.AddCaller(), zap.AddCallerSkip(1))
	} else {
		defaultLogger.logger = zap.New(zapcore.NewTee(consoleCore, infoCore, errorCore),
			zap.AddCaller(), zap.AddCallerSkip(1))
	}
	defaultLogger.sugaredLogger = defaultLogger.logger.Sugar()

	//log.Infof("init log success! path:%s name:%s level:%s", pathName, logName, level)
	return nil
}

func initZapLoggerWithoutConsole(pathName string, logName string, level zapcore.Level) error {
	infoCore, err := getInfoCore(pathName, logName)
	if err != nil {
		return err
	}

	errorCore, err := getErrorCore(pathName, logName)
	if err != nil {
		return err
	}

	if level == zapcore.DebugLevel {
		debugCore, err := getDebugCore(pathName, logName)
		if err != nil {
			return err
		}
		defaultLogger.logger = zap.New(zapcore.NewTee(debugCore, infoCore, errorCore),
			zap.AddCaller(), zap.AddCallerSkip(1))
	} else {
		defaultLogger.logger = zap.New(zapcore.NewTee(infoCore, errorCore),
			zap.AddCaller(), zap.AddCallerSkip(1))
	}
	defaultLogger.sugaredLogger = defaultLogger.logger.Sugar()

	//log.Infof("init log success! path:%s name:%s level:%s", pathName, logName, level)
	return nil
}

func newDefaultEncoderConfig() zapcore.EncoderConfig {
	encoderConfig := zap.NewProductionEncoderConfig()
	encoderConfig.EncodeLevel = zapcore.CapitalLevelEncoder
	encoderConfig.EncodeTime = zapcore.TimeEncoderOfLayout("[2006-01-02 15:04:05.000]")

	return encoderConfig
}

// getConsoleCore 构造屏幕输出zapCore
func getConsoleCore(level zap.AtomicLevel) (zapcore.Core, error) {
	consoleEncoder := newDefaultEncoderConfig()
	consoleEncoder.EncodeLevel = zapcore.CapitalColorLevelEncoder

	return zapcore.NewCore(zapcore.NewConsoleEncoder(consoleEncoder), zapcore.Lock(os.Stdout), level), nil
}

// getDebugCore 构造写Debug日志文件zapCore
func getDebugCore(pathName string, logName string) (zapcore.Core, error) {
	logEncoder := newDefaultEncoderConfig()
	writeSyncer := getLoggerWriter(pathName, logName+"_debug")

	return zapcore.NewCore(zapcore.NewConsoleEncoder(logEncoder), writeSyncer, defaultLogger.atomicLevel), nil
}

// getInfoCore 构造写Info日志文件zapCore
func getInfoCore(pathName string, logName string) (zapcore.Core, error) {
	logEncoder := newDefaultEncoderConfig()
	writeSyncer := getLoggerWriter(pathName, logName+"_info")

	return zapcore.NewCore(zapcore.NewConsoleEncoder(logEncoder), writeSyncer, InfoEnabled{}), nil
}

// getErrorCore 构造写Error日志文件zapCore
func getErrorCore(pathName string, logName string) (zapcore.Core, error) {
	logEncoder := newDefaultEncoderConfig()
	writeSyncer := getLoggerWriter(pathName, logName+"_error")

	return zapcore.NewCore(zapcore.NewJSONEncoder(logEncoder), writeSyncer, ErrorEnabled{}), nil
}

func getLoggerWriter(pathName string, fileName string) zapcore.WriteSyncer {
	return zapcore.AddSync(
		&lumberjack.Logger{
			Filename:   filepath.Join(pathName, fileName+".log"),
			MaxSize:    100, // 单位MB
			MaxBackups: 10,
		})
}
