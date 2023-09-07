package utils

import (
	"runtime"
	"sync"
	"time"
)

// BatchMgr  一个简单的并发批量任务执行管理器，用来等待一堆任务并发执行完成。
type BatchMgr struct {
	WG  sync.WaitGroup
	mut sync.Mutex // Mutex 互斥的效率比chan要高一点

	maxOccurs chan struct{} // 最大并发数量
	occLimit  bool          // 是否有并发限制
	Tasks     []func()
}

func NewBatchMgr(maxOccurs int) *BatchMgr {
	occLimit := true
	if maxOccurs <= 0 {
		maxOccurs = runtime.NumCPU() * 2
		occLimit = false
	}
	return &BatchMgr{
		maxOccurs: make(chan struct{}, maxOccurs),
		occLimit:  occLimit,
		Tasks:     make([]func(), 0, maxOccurs),
	}
}

// AddTask 若Exec已开始，则所有任务执行完毕之前，调用此方法会阻塞。
func (m *BatchMgr) AddTask(fn func()) *BatchMgr {
	m.mut.Lock()
	defer m.mut.Unlock()

	m.Tasks = append(m.Tasks, func() {
		defer m.WG.Done()
		if m.occLimit {
			m.maxOccurs <- struct{}{}
			defer func() {
				<-m.maxOccurs
			}()
		}
		fn()
	})
	return m
}

func (m *BatchMgr) ClearTasks() {
	m.mut.Lock()
	defer m.mut.Unlock()

	m.Tasks = m.Tasks[:0]
}

// Exec 并发执行批量任务，阻塞到所有任务执行完毕为止
func (m *BatchMgr) Exec() {
	m.mut.Lock()
	defer m.mut.Unlock()

	m.WG.Add(len(m.Tasks))
	for _, task := range m.Tasks {
		go task()
	}
	m.WG.Wait()
}

// ExecSingle 按顺序执行任务，非并发
func (m *BatchMgr) ExecSingle() {
	m.mut.Lock()
	defer m.mut.Unlock()

	m.WG.Add(len(m.Tasks))
	for _, task := range m.Tasks {
		task()
	}
}

// LoopWait 每固定时间执行一次条件检测；若条件满足，则执行回调
func LoopWait(timeMs int64, condition func() bool, callback func()) {
	for condition() {
		if callback != nil {
			callback()
		}
		return
	}
	time.AfterFunc(time.Duration(timeMs)*time.Millisecond, func() {
		if condition() {
			if callback != nil {
				callback()
			}
			return
		} else {
			LoopWait(timeMs, condition, callback)
		}
	})
}
