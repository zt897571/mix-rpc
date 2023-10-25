// Package xrpc -----------------------------
// @file      : helper.go
// @author    : zhangtuo
// @contact   :
// @time      : 2023/10/20 11:57
// -------------------------------------------
package xrpc

import (
	"context"
	"golang/error_code"
)

type timeoutChannel[T any] struct {
	channel chan T
}

func newTimeoutChannel[T any](size int32) *timeoutChannel[T] {
	return &timeoutChannel[T]{
		channel: make(chan T, size),
	}
}

func (t *timeoutChannel[T]) getChannel() chan T {
	return t.channel
}

func (t *timeoutChannel[T]) blockRead(ctx context.Context) (T, error) {
	var defultValue T
	select {
	case value := <-t.channel:
		return value, nil
	case <-ctx.Done():
		return defultValue, ctx.Err()
	}
}

func (t *timeoutChannel[T]) write(value T) error {
	select {
	case t.channel <- value:
		return nil
	default:
		return error_code.ChannelInvalid
	}
}
