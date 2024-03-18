{***************************************************************************}
{                                                                           }
{           Spring Benchmark for Delphi                                     }
{                                                                           }
{           Copyright (c) 2021 Spring4D Team                                }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

// This is a port from Google Benchmark: https://github.com/google/benchmark - v1.5.3

unit Spring.Benchmark;

{$T-,X+,H+,B-}
{$IFNDEF DEBUG}{$O+,W-}{$ENDIF}
{$INLINE ON}
{$IFDEF FPC}
  {$IFOPT D+}{$DEFINE DEBUG}{$ENDIF}
  {$MODE DELPHI}
  {$ASMMODE INTEL}
  {$DEFINE HAS_RECORD_FINALIZER}
  {$NOTES OFF}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ELSE}
  {$LEGACYIFEND ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$DEFINE HAS_UNITSCOPE}
  {$IF CompilerVersion < 28.0}
    {$MESSAGE ERROR 'Delphi XE7 or higher required'}
  {$IFEND}
  {$IF CompilerVersion >= 34.0}
    {$DEFINE HAS_RECORD_FINALIZER}
  {$IFEND}
  {$IFDEF ANDROID}
    {$DEFINE APP_LOG}
    {$DEFINE DELPHI_ANDROID}
  {$ENDIF}
  {$IFDEF IOS}
    {$DEFINE APP_LOG}
    {$DEFINE DELPHI_DARWIN}
  {$ENDIF}
  {$IFDEF OSX}
    {$DEFINE APP_LOG}
    {$DEFINE DELPHI_DARWIN}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SyncObjs;
{$ELSE}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  SyncObjs;
{$ENDIF}


type
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  TRTLConditionVariable = record
    Ptr: Pointer;
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  TRTLConditionVariable = pthread_cond_t;
  {$ENDIF}
  TConditionVariable = class
  private
    fConditionVariable: TRTLConditionVariable;
  public
    procedure ReleaseAll;
    procedure WaitFor(criticalSection: TCriticalSection);
  end;
{$ELSE}
  {$IFDEF MSWINDOWS}
  TConditionVariable = TConditionVariableCS;
  {$ELSE}
  TConditionVariable = TConditionVariableMutex;
  TCriticalSection = TMutex;
  {$ENDIF}
{$ENDIF}

  PCounter = ^TCounter;
  TCounter = record
  type
    TFlag = (
      // Mark the counter as a rate. It will be presented divided
      // by the duration of the benchmark.
      kIsRate,
      // Mark the counter as a thread-average quantity. It will be
      // presented divided by the number of threads.
      kAvgThreads,
      // Mark the counter as a constant value, valid/same for *every* iteration.
      // When reporting, it will be *multiplied* by the iteration count.
      kIsIterationInvariant,
      // Mark the counter as a iteration-average quantity.
      // It will be presented divided by the number of iterations.
      kAvgIterations,

      // In the end, invert the result. This is always done last!
      kInvert
    );
    TFlags = set of TFlag;
  type
    TOneK = (
      // 1'000 items per 1k
      kIs1000,
      // 1'024 items per 1k
      kIs1024
    );
  public
    value: Double;
    flags: TFlags;
    oneK: TOneK;
    procedure Init(v: Double = 0.0; f: TFlags = []; k: TOneK = kIs1000); inline;
    class operator Implicit(const value: Double): TCounter;
  end;

const
  // Mark the counter as a thread-average rate. See above.
  kAvgThreadsRate = [kIsRate, kAvgThreads];
  // Mark the counter as a constant rate.
  // When reporting, it will be *multiplied* by the iteration count
  // and then divided by the duration of the benchmark.
  kIsIterationInvariantRate = [kIsRate, kIsIterationInvariant];
  // Mark the counter as a iteration-average rate. See above.
  kAvgIterationsRate = [kIsRate, kAvgIterations];

type
  TUserCounter = record name: string; counter: TCounter end;
  TUserCounters = array of TUserCounter;
  TUserCountersHelper = record helper for TUserCounters
    function Find(const name: string): PCounter;
    function Get(const name: string): PCounter;
  end;

  PCounterStat = ^TCounterStat;
  TCounterStat = record c: TCounter; s: TArray<Double>; end;
  TCounterStatsItem = record name: string; counter: TCounterStat end;
  TCounterStats = array of TCounterStatsItem;
  TCounterStatsHelper = record helper for TCounterStats
    function Find(const name: string): PCounterStat;
    function Get(const name: string): PCounterStat;
  end;

  TBigO = (oNone, o1, oN, oNSquared, oNCubed, oLogN, oNLogN, oAuto, oLambda);

  TIterationCount = type UInt64;

  TBigOFunc = {$IFNDEF FPC}reference to{$ENDIF} function(const n: TIterationCount): Double;

  TStatisticsFunc = {$IFNDEF FPC}reference to{$ENDIF} function(const values: array of Double): Double;

  TStatistics = record
    Name: string;
    Compute: TStatisticsFunc;
    constructor Create(const name: string; const compute: TStatisticsFunc);
  end;

  TThreadTimer = class
  strict private
    // should the thread, or the process, time be measured?
    fMeasureProcessCpuTime: Boolean;
    fRunning: Boolean;

    fStartRealTime,
    fStartCpuTime,

    // Accumulated time so far (does not contain current slice if running)
    fRealTimeUsed,
    fCpuTimeUsed,
    // Manually set iteration time. User sets this with SetIterationTime(seconds).
    fManualTimeUsed: Double;

    function GetRealTimeRunning: Double; inline;
    function GetCpuTimeUsed: Double; inline;
    function GetManualTimeUsed: Double; inline;
    function ReadCpuTimerOfChoice: Double;
  private
    constructor Create(measureProcessCpuTime: Boolean);
  public
    constructor CreateProcessCpuTime;

    // Called by each thread
    procedure StartTimer;

    // Called by each thread
    procedure StopTimer;

    // Called by each thread
    procedure SetIterationTime(seconds: Double);

    property Running: Boolean read fRunning;

    // REQUIRES: timer is not running
    property RealTimeUsed: Double read GetRealTimeRunning;

    // REQUIRES: timer is not running
    property CpuTimeUsed: Double read GetCpuTimeUsed;

    // REQUIRES: timer is not running
    property ManualTimeUsed: Double read GetManualTimeUsed;
  end;

  TBarrier = class
  private
    fLock: TCriticalSection;
    fPhaseCondition: TConditionVariable;
    fRunningThreads: Integer;
    fPhaseNumber: Integer;
    fEntered: Integer;

    function CreateBarrier: Boolean;
  public
    constructor Create(numThreads: Integer);
    destructor Destroy; override;
    function Wait: Boolean;
    procedure RemoveThread;
  end;

  TThreadManager = class
  type
    TResult = record
      iterations: TIterationCount;
      realTimeUsed: Double;
      cpuTimeUsed: Double;
      manualTimeUsed: Double;
      complexityN: UInt64;
      reportLabel: string;
      errorMessage: string;
      hasError: Boolean;
      counters: TUserCounters;
    end;
  private
    fBenchmarkLock: TCriticalSection;
    fAliveThreads: Integer;
    fStartStopBarrier: TBarrier;
    fEndCondLock: TCriticalSection;
    fEndCondition: TConditionVariable;
  public
    results: TResult;
    constructor Create(const numThreads: Integer);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function StartStopBarrier: Boolean; inline;
    procedure NotifyThreadComplete;
    procedure WaitForAllThreads;
  end;

  TAggregationReportMode = (
    // The mode is user-specified.
    // This may or may not be set when the following bit-flags are set.
    armDefault,
    // File reporter should only output aggregates.
    armFileReportAggregatesOnly,
    // Display reporter should only output aggregates
    armDisplayReportAggregatesOnly
  );
  TAggregationReportModes = set of TAggregationReportMode;
const
  armUnspecified = [];
    // Both reporters should only display aggregates.
  armReportAggregatesOnly = [armFileReportAggregatesOnly, armDisplayReportAggregatesOnly];

type
  // State is passed to a running Benchmark and contains state for the
  // benchmark to use.
  TState = record
  type
    TValue = record end;
    TStateIterator = record
    private
      fCached: TIterationCount;
      fParent: ^TState;
      fCurrent: TValue;
    public
      function MoveNext: Boolean; inline;
    {$IFDEF HAS_RECORD_FINALIZER}
    {$IFDEF FPC}
      class operator Initialize(var iter: TStateIterator);
    {$ENDIF}
      class operator Finalize(var iter: TStateIterator);
    {$ENDIF}
      property Current: TValue read fCurrent;
    end;
  strict private
    // When total_iterations_ is 0, KeepRunning() and friends will return false.
    // May be larger than max_iterations.
    fTotalIterations,

    // When using KeepRunningBatch(), batch_leftover_ holds the number of
    // iterations beyond max_iters that were run. Used to track
    // completed_iterations_ accurately.
    fBatchLeftover,

    fMaxIterations: TIterationCount;
    fStarted, fFinished, fErrorOccurred: Boolean;

    fRange: TArray<Int64>;
    fComplexityN: UInt64;
    fTimer: TThreadTimer;
    fManager: TThreadManager;
    fThreadIndex: Integer;
    fThreads: Integer;

    function GetBytesProcessed: UInt64;
    function GetItemsProcessed: UInt64;
    function GetIterations: TIterationCount;
    function GetRange(index: Integer): Int64; inline;
    function GetCounter(const name: string): TCounter;
    procedure SetCounter(const name: string; const value: TCounter);
    procedure StartKeepRunning;
    // Implementation of KeepRunning() and KeepRunningBatch().
    // isBatch must be true unless n is 1.
    function KeepRunningInternal(const n: TIterationCount; isBatch: Boolean): Boolean; inline;
    procedure FinishKeepRunning; {$IFDEF HAS_RECORD_FINALIZER}inline;{$ENDIF}
  private
    // Container for user-defined counters.
    fCounters: TUserCounters;

    constructor Create(const maxIters: TIterationCount; const ranges: TArray<Int64>;
      threadId, threadCount: Integer; const timer: TThreadTimer;
      const manager: TThreadManager);
    property MaxIterations: TIterationCount read fMaxIterations;
  public
    // Index of the executing thread. Values from [0, threads).
    property ThreadIndex: Integer read fThreadIndex;

    // Number of threads concurrently executing the benchmark.
    property Threads: Integer read fThreads;

    // Returns iterator used to run each iteration of a benchmark using a
    // for-in loop. This function should not be called directly.
    //
    // REQUIRES: The benchmark has not started running yet. Neither begin nor end
    // have been called previously.
    //
    // NOTE: KeepRunning may not be used after calling either of these functions.
    function GetEnumerator: TStateIterator; inline;

    // Returns true if the benchmark should continue through another iteration.
    // NOTE: A benchmark may not return from the test until KeepRunning() has
    // returned false.
    function KeepRunning: Boolean; inline;

    // Returns true if the benchmark should run n more iterations.
    // REQUIRES: 'n' > 0.
    // NOTE: A benchmark must not return from the test until KeepRunningBatch()
    // has returned false.
    // NOTE: KeepRunningBatch() may overshoot by up to 'n' iterations.
    //
    // Intended usage:
    //   while state.KeepRunningBatch(1000) do
    //     // process 1000 elements
    function KeepRunningBatch(const n: TIterationCount): Boolean; inline;

    // REQUIRES: timer is running and 'SkipWithError(...)' has not been called
    //           by the current thread.
    // Stop the benchmark timer.  If not called, the timer will be
    // automatically stopped after the last iteration of the benchmark loop.
    //
    // For threaded benchmarks the PauseTiming() function only pauses the timing
    // for the current thread.
    //
    // NOTE: The "real time" measurement is per-thread. If different threads
    // report different measurements the largest one is reported.
    //
    // NOTE: PauseTiming()/ResumeTiming() are relatively
    // heavyweight, and so their use should generally be avoided
    // within each benchmark iteration, if possible.
    procedure PauseTiming; inline;

    // REQUIRES: timer is not running and 'SkipWithError(...)' has not been called
    //           by the current thread.
    // Start the benchmark timer.  The timer is NOT running on entrance to the
    // benchmark function. It begins running after control flow enters the
    // benchmark loop.
    //
    // NOTE: PauseTiming()/ResumeTiming() are relatively
    // heavyweight, and so their use should generally be avoided
    // within each benchmark iteration, if possible.
    procedure ResumeTiming; inline;

    // REQUIRES: 'SkipWithError(...)' has not been called previously by the
    //            current thread.
    // Report the benchmark as resulting in an error with the specified 'msg'.
    // After this call the user may explicitly 'return' from the benchmark.
    //
    // If the ranged-for style of benchmark loop is used, the user must explicitly
    // break from the loop, otherwise all future iterations will be run.
    // If the 'KeepRunning()' loop is used the current thread will automatically
    // exit the loop at the end of the current iteration.
    //
    // For threaded benchmarks only the current thread stops executing and future
    // calls to `KeepRunning()` will block until all threads have completed
    // the `KeepRunning()` loop. If multiple threads report an error only the
    // first error message is used.
    //
    // NOTE: Calling 'SkipWithError(...)' does not cause the benchmark to exit
    // the current scope immediately. If the function is called from within
    // the 'KeepRunning()' loop the current iteration will finish. It is the users
    // responsibility to exit the scope as needed.
    procedure SkipWithError(const msg: string);

    // Returns true if an error has been reported with 'SkipWithError(...)'.
    property ErrorOccurred: Boolean read fErrorOccurred;

    // REQUIRES: called exactly once per iteration of the benchmarking loop.
    // Set the manually measured time for this benchmark iteration, which
    // is used instead of automatically measured time if UseManualTime() was
    // specified.
    //
    // For threaded benchmarks the final value will be set to the largest
    // reported values.
    procedure SetIterationTime(const seconds: Double);

    // Set the number of bytes processed by the current benchmark
    // execution.  This routine is typically called once at the end of a
    // throughput oriented benchmark.
    //
    // REQUIRES: a benchmark has exited its benchmarking loop.
    procedure SetBytesProcessed(const bytes: UInt64);

    // If this routine is called with complexityN > 0 and complexity report is
    // requested for the
    // family benchmark, then current benchmark will be part of the computation
    // and complexityN will
    // represent the length of N.
    procedure SetComplexityN(const complexityN: UInt64); inline;

    // If this routine is called with items > 0, then an items/s
    // label is printed on the benchmark report line for the currently
    // executing benchmark. It is typically called at the end of a processing
    // benchmark where a processing items/second output is desired.
    //
    // REQUIRES: a benchmark has exited its benchmarking loop.
    procedure SetItemsProcessed(const items: UInt64);

    // If this routine is called, the specified label is printed at the
    // end of the benchmark report line for the currently executing
    // benchmark.  Example:
    //  procedure BM_Compress(const state: TState);
    //  begin
    //    ...
    //    var compress = input_size / output_size;
    //    state.SetLabel(Format('compress:%.1f%%', 100.0 * compression));
    //  end;
    // Produces output that looks like:
    //  BM_Compress   50         50   14115038  compress:27.3%
    //
    // REQUIRES: a benchmark has exited its benchmarking loop.
    procedure SetLabel(const text: string);

    property BytesProcessed: UInt64 read GetBytesProcessed;
    property ComplexityN: UInt64 read fComplexityN;
    property ItemsProcessed: UInt64 read GetItemsProcessed;
    property Iterations: TIterationCount read GetIterations;

    property Counters[const name: string]: TCounter read GetCounter write SetCounter;

    // Range arguments for this run. CHECKs if the argument has been set.
    property Range[index: Integer]: Int64 read GetRange; default;
  end;

  TFunction = procedure(const state: TState);

  TTimeUnit = (kNanosecond, kMicrosecond, kMillisecond, kSecond);

  // ------------------------------------------------------
  // Benchmark registration object.  The Benchmark() function returns this.
  // Various methods can be called on this object to change the properties of
  // the benchmark. Each method returns "this" so that multiple method calls
  // can chained into one expression.
  TBenchmark = class
  type
    TRange = record start, limit: Int64 end;
    TCustomizeFunc = {$IFNDEF FPC}reference to{$ENDIF} procedure(const benchmark: TBenchmark);
  private
    fName: string;
    fAggregationReportMode: TAggregationReportModes;
    fArgNames: TArray<string>;
    fArgs: TArray<TArray<Int64>>;
    fTimeUnit: TTimeUnit;
    fRangeMultiplier: Integer;
    fMinTime: Double;
    fIterations: TIterationCount;
    fRepetitions: Integer;
    fMeasureProcessCpuTime,
    fUseRealTime,
    fUseManualTime: Boolean;
    fComplexity: TBigO;
    fComplexityLambda: TBigOFunc;
    fStatistics: TArray<TStatistics>;
    fThreadCounts: TArray<Integer>;
    function GetArgsCount: Integer;
  strict protected
    constructor Create(const name: string);
  public
    procedure Run(const state: TState); virtual; abstract;

    // Note: the following methods all return "Self" so that multiple
    // method calls can be chained together in one expression.

    // Run this benchmark once with "x" as the extra argument passed
    // to the function.
    // REQUIRES: The function passed to the constructor must accept an arg1.
    function Arg(const x: Int64): TBenchmark;

     // Run this benchmark with the given time unit for the generated output report
    function TimeUnit(const timeUnit: TTimeUnit): TBenchmark;

    // Run this benchmark once for a number of values picked from the
    // range [start..limit].  (start and limit are always picked.)
    // REQUIRES: The function passed to the constructor must accept an arg1.
    function Range(const start, limit: Int64): TBenchmark;

    // Run this benchmark once for all values in the range [start..limit] with
    // specific step
    // REQUIRES: The function passed to the constructor must accept an arg1.
    function DenseRange(const start, limit: Int64; step: Integer = 1): TBenchmark;

    // Run this benchmark once with "args" as the extra arguments passed
    // to the function.
    // REQUIRES: The function passed to the constructor must accept arg1, arg2 ...
    function Args(const args: array of Int64): TBenchmark;

    // Run this benchmark once for a number of values picked from the
    // ranges [start..limit].  (starts and limits are always picked.)
    // REQUIRES: The function passed to the constructor must accept arg1, arg2 ...
    function Ranges(const ranges: array of TRange): TBenchmark;

    // Run this benchmark once for each combination of values in the (cartesian)
    // product of the supplied argument lists.
    // REQUIRES: The function passed to the constructor must accept arg1, arg2 ...
    function ArgsProduct(const argLists: TArray<TArray<Int64>>): TBenchmark;

    // Equivalent to ArgNames([name])
    function ArgName(const name: string): TBenchmark;

    // Set the argument names to display in the benchmark name. If not called,
    // only argument values will be shown.
    function ArgNames(const names: array of string): TBenchmark;

    // Pass this benchmark object to customArguments, which can customize
    // the benchmark by calling various methods like Arg, Args,
    // Threads, etc.
    function Apply(const customArguments: TCustomizeFunc): TBenchmark;

    // Set the range multiplier for non-dense range. If not called, the range
    // multiplier kRangeMultiplier will be used.
    function RangeMultiplier(const multiplier: Integer): TBenchmark;

    // Set the minimum amount of time to use when running this benchmark. This
    // option overrides the `benchmark_min_time` flag.
    // REQUIRES: `t > 0` and `Iterations` has not been called on this benchmark.
    function MinTime(const t: Double): TBenchmark;

    // Specify the amount of iterations that should be run by this benchmark.
    // REQUIRES: 'n > 0' and `MinTime` has not been called on this benchmark.
    //
    // NOTE: This function should only be used when *exact* iteration control is
    //   needed and never to control or limit how long a benchmark runs, where
    // `--benchmark_min_time=N` or `MinTime(...)` should be used instead.
    function Iterations(const n: TIterationCount): TBenchmark;

    // Specify the amount of times to repeat this benchmark. This option overrides
    // the `benchmark_repetitions` flag.
    // REQUIRES: `n > 0`
    function Repetitions(const n: Integer): TBenchmark;

    // Specify if each repetition of the benchmark should be reported separately
    // or if only the final statistics should be reported. If the benchmark
    // is not repeated then the single result is always reported.
    // Applies to *ALL* reporters (display and file).
    function ReportAggregatesOnly(const value: Boolean = True): TBenchmark;

    // Same as ReportAggregatesOnly(), but applies to display reporter only.
    function DisplayAggregatesOnly(const value: Boolean = True): TBenchmark;

    // By default, the CPU time is measured only for the main thread, which may
    // be unrepresentative if the benchmark uses threads internally. If called,
    // the total CPU time spent by all the threads will be measured instead.
    // By default, the only the main thread CPU time will be measured.
    function MeasureProcessCPUTime: TBenchmark;

    // If a particular benchmark should use the Wall clock instead of the CPU time
    // (be it either the CPU time of the main thread only (default), or the
    // total CPU usage of the benchmark), call this method. If called, the elapsed
    // (wall) time will be used to control how many iterations are run, and in the
    // printing of items/second or MB/seconds values.
    // If not called, the CPU time used by the benchmark will be used.
    function UseRealTime: TBenchmark;

    // If a benchmark must measure time manually (e.g. if GPU execution time is
    // being
    // measured), call this method. If called, each benchmark iteration should
    // call
    // SetIterationTime(seconds) to report the measured time, which will be used
    // to control how many iterations are run, and in the printing of items/second
    // or MB/second values.
    function UseManualTime: TBenchmark;

    // Set the asymptotic computational complexity for the benchmark. If called
    // the asymptotic computational complexity will be shown on the output.
    function Complexity(const complexity: TBigO = oAuto): TBenchmark; overload;

    // Set the asymptotic computational complexity for the benchmark. If called
    // the asymptotic computational complexity will be shown on the output.
    function Complexity(const complexity: TBigOFunc): TBenchmark; overload;

    // Add this statistics to be computed over all the values of benchmark run
    function ComputeStatistics(const name: string; const statistics: TStatisticsFunc): TBenchmark;

    // Support for running multiple copies of the same benchmark concurrently
    // in multiple threads.  This may be useful when measuring the scaling
    // of some piece of code.

    // Run one instance of this benchmark concurrently in t threads.
    function Threads(const t: Integer): TBenchmark;

    property ArgsCount: Integer read GetArgsCount;
  end;

procedure Benchmark_Main(pinThread0: Boolean = False);
function Benchmark(const fn: TFunction; const name: string): TBenchmark;
function Range(const start, limit: Int64): TBenchmark.TRange;
function Counter(const value: Double; flags: TCounter.TFlags = []; k: TCounter.TOneK = kIs1000): TCounter; inline;


{$REGION 'Global settings'}

// The following variables are being set via commandline parameters
// but can also be set in code.

var
  // Print a list of benchmarks. This option overrides all other options.
  benchmark_list_tests: Boolean = False;

  // A regular expression that specifies the set of benchmarks to execute.  If
  // this flag is empty, or if this flag is the string \"all\", all benchmarks
  // linked into the binary are run.
  benchmark_filter: string = '.';

  // Minimum number of seconds we should run benchmark before results are
  // considered significant.  For cpu-time based tests, this is the lower bound
  // on the total cpu time used by all threads that make up the test.  For
  // real-time based tests, this is the lower bound on the elapsed time of the
  // benchmark execution, regardless of number of threads.
  benchmark_min_time: Double = 0.5;

  // The number of runs of each benchmark. If greater than 1, the mean and
  // standard deviation of the runs will be reported.
  benchmark_repetitions: Integer = 1;

  // Report the result of each benchmark repetitions. When 'true' is specified
  // only the mean, standard deviation, and other statistics are reported for
  // repeated benchmarks. Affects all reporters.
  benchmark_report_aggregates_only: Boolean = False;

  // Display the result of each benchmark repetitions. When 'true' is specified
  // only the mean, standard deviation, and other statistics are displayed for
  // repeated benchmarks. Unlike benchmark_report_aggregates_only, only affects
  // the display reporter, but *NOT* file reporter, which will still contain
  // all the output.
  benchmark_display_aggregates_only: Boolean = False;

  // The format to use for console output.
  // Valid values are 'console', 'json', or 'csv'.
  benchmark_format: string = 'console';

  // The format to use for file output.
  // Valid values are 'console', 'json', or 'csv'.
  benchmark_out_format: string = 'json';

  // The file to write additional output to.
  benchmark_out: string = '';

  // Whether to use colors in the output.  Valid values:
  // 'true'/'yes'/1, 'false'/'no'/0, and 'auto'. 'auto' means to use colors if
  // the output is being sent to a terminal and the TERM environment variable is
  // set to a terminal type that supports colors.
  benchmark_color: string = 'auto';

  // Whether to use tabular format when printing user counters to the console.
  // Valid values: 'true'/'yes'/1, 'false'/'no'/0.  Defaults to false.
  benchmark_counters_tabular: Boolean = False;

  // The level of verbose logging to output
  log_level: Integer = 0;

  // separator used for the csv file
  csv_separator: Char = ';';

{$ENDREGION}

implementation

uses
{$IFDEF HAS_UNITSCOPE}
{$IFDEF MSWINDOWS}
  Winapi.ShLwApi,
  Winapi.Windows,
{$ELSE}
  Posix.Base,
  Posix.Stdlib,
  Posix.SysTime,
  Posix.Time,
  Posix.Unistd,
  Posix.Fcntl,
  Posix.SysSysctl,
  Posix.StdDef,
{$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Androidapi.Log,
{$ENDIF}
{$IFDEF IOS}
  Macapi.Mach,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSapi.Foundation,
{$ENDIF}
{$IFDEF OSX}
  Macapi.Mach,
  Macapi.Helpers,
  Macapi.ObjectiveC,
  Macapi.Foundation,
{$ENDIF}
{$ENDIF}
  System.Classes,
  System.Character,
  System.DateUtils,
  System.Math,
  System.RegularExpressions,
  System.StrUtils,
  System.SysUtils;
{$ELSE}
  Classes,
  Character,
  DateUtils,
  Math,
  RegExpr,
  StrUtils,
  SysUtils;
{$ENDIF}


type
  TCPUInfo = record
  type
    TCacheInfo = record
      typ: string;
      level: Cardinal;
      size: Cardinal;
      numSharing: Integer;
    end;
    TScaling = (Unknown, Enabled, Disabled);
  public class var
    numCpus: Integer;
    cyclesPerSecond: Double;
    caches: TArray<TCacheInfo>;
    scaling: TScaling;
    loadAvg: TArray<Double>;
    class constructor Create;
  end;

  TFunctionBenchmark = class(TBenchmark)
  strict private
    fFunc: TFunction;
  public
    constructor Create(const name: string; const func: TFunction);

    procedure Run(const state: TState); override;
  end;

  TBenchmarkName = record
    functionName: string;
    args: string;
    minTime: string;
    iterations: string;
    pepetitions: string;
    timeType: string;
    threads: string;

    function Str: string;
  end;

  TBenchmarkInstance = record
    name: TBenchmarkName;
    benchmark: TBenchmark;
    aggregationReportMode: TAggregationReportModes;
    arg: TArray<Int64>;
    timeUnit: TTimeUnit;
    rangeMultiplier: Integer;
    measureProcessCpuTime,
    useRealTime,
    useManualTime: Boolean;
    complexity: TBigO;
    complexityLambda: TBigOFunc;
    counters: TUserCounters;
    statistics: TArray<TStatistics>;
    lastBenchmarkInstance: Boolean;
    repetitions: Integer;
    minTime: Double;
    iterations: TIterationCount;
    threads: Cardinal;

    function Run(const iters: TIterationCount; threadId: Integer;
      const timer: TThreadTimer; const manager: TThreadManager): TState;
  end;

  TBenchmarkFamilies = class
  strict private class var
    fFamilies: TList;
    fLock: TCriticalSection;
    class constructor Create;
    class destructor Destroy;
  public
    class function AddBenchmark(const family: TBenchmark): Integer;
    class procedure ClearBenchmarks;
    class function FindBenchmarks(spec: string;
      var benchmarks: TArray<TBenchmarkInstance>): Boolean;
  end;

  TBenchmarkReporter = class
  type
    TContext = record
      // The number of chars in the longest benchmark name.
      nameFieldWidth: NativeUInt;
      class var executableName: string;
    end;
    TRun = record
      const noRepetitionIndex = -1;
      type TRunType = (rtIteration, rtAggregate);

    strict private
      function GetBenchmarkName: string;
    public
      runName: TBenchmarkName;
      runType: TRunType;
      aggregateName: string;
      reportLabel: string;
      errorOccurred: Boolean;
      errorMessage: string;
      iterations: TIterationCount;
      threads: UInt64;
      repetitionIndex: Int64;
      repetitions: Int64;
      timeUnit: TTimeUnit;
      realAccumulatedTime: Double;
      cpuAccumulatedTime: Double;
      maxHeapBytesUsed: Double;
      complexity: TBigO;
      complexityLambda: TBigOFunc;
      complexityN: UInt64;
      statistics: TArray<TStatistics>;
      reportBigO: Boolean;
      reportRms: Boolean;
      counters: TUserCounters;
      hasMemoryResult: Boolean;
      allocsPerIter: Double;
      maxBytesUsed: UInt64;

      class function Create: TRun; static;
      function GetAdjustedRealTime: Double;
      function GetAdjustedCPUTime: Double;
      property BenchmarkName: string read GetBenchmarkName;
    end;
  strict protected
    fOutputStream: TStream;
  public
    function ReportContext(const context: TContext): Boolean; virtual; abstract;
    procedure ReportRuns(const reports: TArray<TRun>); virtual; abstract;
    procedure PrintBasicContext(var output: System.Text; const context: TContext);
    procedure Finalize; virtual;

    property OutputStream: TStream write fOutputStream;
  end;

  TConsoleReporter = class(TBenchmarkReporter)
  type
    TOutputOption = (
      ooColor,
      ooTabular
    );
    TOutputOptions = set of TOutputOption;
  const
    ooNone = [];
    ooColorTabular = [ooColor, ooTabular];
    ooDefaults = ooColorTabular;

  protected
    fOutputOptions: TOutputOptions;
    fNameFieldWidth: Integer;
    fPrevCounters: TUserCounters;
    fPrintedHeader: Boolean;
    procedure PrintRunData(const result: TBenchmarkReporter.TRun);
    procedure PrintHeader(const run: TBenchmarkReporter.TRun);
  public
    constructor Create(const opts: TOutputOptions);
    function ReportContext(const context: TBenchmarkReporter.TContext): Boolean; override;
    procedure ReportRuns(const reports: TArray<TBenchmarkReporter.TRun>); override;
  end;

  TCSVReporter = class(TBenchmarkReporter)
  private
    fPrintedHeader: Boolean;
    fUserCounterNames: TArray<string>;
    procedure PrintRunData(const run: TBenchmarkReporter.TRun);
  public
    function ReportContext(const context: TBenchmarkReporter.TContext): Boolean; override;
    procedure ReportRuns(const reports: TArray<TBenchmarkReporter.TRun>); override;
  end;

  TJSONReporter = class(TBenchmarkReporter)
  private
    fFirstReport: Boolean;
    procedure PrintRunData(const run: TBenchmarkReporter.TRun);
  public
    constructor Create;
    function ReportContext(const context: TBenchmarkReporter.TContext): Boolean; override;
    procedure ReportRuns(const reports: TArray<TBenchmarkReporter.TRun>); override;
    procedure Finalize; override;
  end;

  TBenchmarkRunner = record
  type
    TRunResults = record
      nonAggregates: TArray<TBenchmarkReporter.TRun>;
      aggregatesOnly: TArray<TBenchmarkReporter.TRun>;
      displayReportAggregatesOnly: Boolean;
      fileReportAggregatesOnly: Boolean;
    end;
  private
    runResults: TRunResults;
    benchmark: TBenchmarkInstance;
    complexityReports: ^TArray<TBenchmarkReporter.TRun>;
    minTime: Double;
    repeats: Integer;
    hasExplicitIterationCount: Boolean;
    pool: TArray<TThread>;
    iters: TIterationCount;

    type TIterationResults = record
      results: TThreadManager.TResult;
      iters: TIterationCount;
      seconds: Double;
    end;
    function DoNIterations: TIterationResults;
    function PredictNumItersNeeded(const i: TIterationResults): TIterationCount;
    function ShouldReportIterationResults(const i: TIterationResults): Boolean;
    procedure DoOneRepetition(repetitionIndex: Int64);
  public
    constructor Create(const benchmark: TBenchmarkInstance;
      var complexityReports: TArray<TBenchmarkReporter.TRun>);
    function GetResults: TRunResults;
  end;

const
  kRangeMultiplier = 8;
  kMaxFamilySize = 100;
  kMaxIterations = 1000000000;


{$REGION 'Freepascal Support'}

{$IFDEF FPC}

{$IFDEF MSWINDOWS}
const
  kernel32 = 'kernel32.dll';

type
  TLogicalProcessorRelationship = (RelationProcessorCore, RelationNumaNode,
    RelationCache, RelationProcessorPackage, RelationGroup, RelationAll = $FFFF);
  TProcessorCacheType = (CacheUnified, CacheInstruction, CacheData, CacheTrace);

  TCacheDescriptor = record
    Level: BYTE;
    Associativity: BYTE;
    LineSize: WORD;
    Size: DWORD;
    _Type: TProcessorCacheType;
  end;
  PCacheDescriptor = ^TCacheDescriptor;

  TSystemLogicalProcessorInformation = record
    ProcessorMask: ULONG_PTR;
    Relationship: TLogicalProcessorRelationship;
    case Integer of
      0: (Flags: BYTE);
      1: (NodeNumber: DWORD);
      2: (Cache: TCacheDescriptor);
      3: (Reserved: array [0..1] of ULONGLONG);
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

function GetLogicalProcessorInformation(Buffer: PSystemLogicalProcessorInformation;
  var ReturnedLength: DWORD): BOOL; stdcall; external kernel32 name 'GetLogicalProcessorInformation';

procedure WakeAllConditionVariable(var ConditionVariable: TRTLConditionVariable); stdcall;
  external kernel32 name 'WakeAllConditionVariable';

function SleepConditionVariableCS(var ConditionVariable: TRTLConditionVariable;
  var CriticalSection: TRTLCriticalSection; dwMilliseconds: DWORD): BOOL; stdcall;
  external kernel32 name 'SleepConditionVariableCS';
{$ENDIF}

type
  TCriticalSectionHelper = class(TSynchroObject)
    FSection: TRTLCriticalSection;
  end;

procedure TConditionVariable.ReleaseAll;
begin
  {$IFDEF MSWINDOWS}
  WakeAllConditionVariable(fConditionVariable);
  {$ENDIF}
  {$IFDEF UNIX}
  pthread_cond_broadcast(fConditionVariable);
  {$ENDIF}
end;

procedure TConditionVariable.WaitFor(criticalSection: TCriticalSection);
begin
  {$IFDEF MSWINDOWS}
  SleepConditionVariableCS(fConditionVariable, TCriticalSectionHelper(criticalSection).FSection, INFINITE);
  {$ENDIF}
  {$IFDEF UNIX}
  pthread_cond_wait(fConditionVariable, TCriticalSectionHelper(criticalSection).FSection);
  {$ENDIF}
end;

type
  TCharHelper = record helper for Char
    function IsLetterOrDigit: Boolean;
  end;

  TRegEx = class(TRegExpr)
    function IsMatch(const Input: string): Boolean; inline;
  end;

function TCharHelper.IsLetterOrDigit: Boolean;
begin
  Result := TCharacter.IsLetterOrDigit(Self);
end;

function TRegEx.IsMatch(const Input: string): Boolean;
begin
  Result := Exec(Input);
end;

function AtomicDecrement(var target: Integer): Integer; external name 'FPC_INTERLOCKEDDECREMENT';

{$ENDIF}

{$ENDREGION}


{$REGION 'Colored and formatted output for console'}

{$IFDEF APP_LOG}
var
  writeCache: string;
{$ENDIF}

// ported from: colorprint.h

type
  TColor = (
    clDefault,
    clRed,
    clGreen,
    clYellow,
    clBlue,
    clMagenta,
    clCyan,
    clWhite
  );

procedure Write(const msg: string; const args: array of const; color: TColor = clWhite); overload;
const
{$IFDEF MSWINDOWS}
  PlatformColorCodes: array[TColor] of Word = (
    FOREGROUND_INTENSITY,
    FOREGROUND_RED,
    FOREGROUND_GREEN,
    FOREGROUND_RED or FOREGROUND_GREEN,
    FOREGROUND_BLUE,
    FOREGROUND_BLUE or FOREGROUND_RED,
    FOREGROUND_BLUE or FOREGROUND_GREEN,
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY
  );
{$ELSE}
  PlatformColorCodes: array[TColor] of Char = (#0, '1', '2', '3', '4', '5', '6', '7');
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  stdOutHandle: THandle;
  bufferInfo: TConsoleScreenBufferInfo;
  oldColorAttributes: Word;
begin
  stdOutHandle := GetStdHandle(STD_OUTPUT_HANDLE);

  // Gets the current text color.
  GetConsoleScreenBufferInfo(stdOutHandle, bufferInfo);
  oldColorAttributes := bufferInfo.wAttributes;

  if oldColorAttributes < 16 then
  begin
    // We need to flush the stream buffers into the console before each
    // SetConsoleTextAttribute call lest it affect the text that is already
    // printed but has not yet reached the console.
    Flush(Output);
    SetConsoleTextAttribute(stdOutHandle, PlatformColorCodes[color]);
  end;

  System.Write(Format(msg, args));
  Flush(Output);

  // Restores the text color.
  if oldColorAttributes < 16 then
    SetConsoleTextAttribute(stdOutHandle, oldColorAttributes);
end;
{$ELSE}
{$IFDEF APP_LOG}
var
  s: string;
begin
  s :=  Format(msg, args);
  writeCache := writeCache + s;
end;
{$ELSE}
var
  colorCode: string;
begin
  colorCode := PlatformColorCodes[color];
  if colorCode <> #0 then
    System.Write(Format(#27'[0;3%sm', [colorCode]));
  System.Write(Format(msg, args) + #27'[m');
end;
{$ENDIF}
{$ENDIF}

procedure Write(const msg: string; color: TColor = clWhite); inline; overload;
begin
  Write(msg, [], color);
end;

procedure WriteLine(const msg: string; const args: array of const; color: TColor = clWhite); overload;
begin
  Write(msg + sLineBreak, args, color);
{$IFDEF APP_LOG}
{$IFDEF DELPHI_ANDROID}
  LOGI(PUtf8Char(Utf8String(writeCache)));
{$ENDIF}
{$IFDEF DELPHI_DARWIN}
  NSLog(StringToID(writeCache));
{$ENDIF}
  writeCache := '';
{$ENDIF}
end;

procedure WriteLine(const msg: string = ''; color: TColor = clWhite); inline; overload;
begin
  Write(msg + sLineBreak, [], color);
{$IFDEF APP_LOG}
{$IFDEF DELPHI_ANDROID}
  LOGI(PUtf8Char(Utf8String(writeCache)));
{$ENDIF}
{$IFDEF DELPHI_DARWIN}
  NSLog(StringToID(writeCache));
{$ENDIF}
  writeCache := '';
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'Commandline flags'}

// ported from benchmark.cc, commandlineflags.cc

// Parses a string as a command line flag.  The string should have
// the format "--flag=value".  When defOptional is true, the "=value"
// part can be omitted.
//
// Returns the value of the flag, or nullptr if the parsing failed.
function ParseFlagValue(const str, flag: string; defOptional: Boolean): PChar;
var
  flagStr: string;
  flagLen: Integer;
begin
  // str and flag must not be empty.
  if (str = '') or (flag = '') then Exit(nil);

  // The flag must start with "--".
  flagStr := '--' + flag;
  flagLen := Length(flagStr);
  if not str.StartsWith(flagStr) then Exit(nil);

  // When defOptional is true, it's OK to not have a "=value" part.
  if defOptional and (Length(str) = flagLen) then Exit(PChar(@str[flagLen]) + 1);

  // If defOptional is true and there are more characters after the
  // flag name, or if defOptional is false, there must be a '=' after
  // the flag name.
  if str[flagLen + 1] <> '=' then Exit(nil);

  // Returns the string after "=".
  Result := PChar(@str[flagLen + 1]) + 1;
end;

function IsFlag(const str, flag: string): Boolean;
begin
  Result := ParseFlagValue(str, flag, True) <> nil;
end;

function IsTruthyFlagValue(const value: string): Boolean;
var
  v: Char;
  valueLower: string;
begin
  if Length(value) = 1 then
  begin
    v := value[1];
    Result := v.IsLetterOrDigit and
              not ((v = '0') or (v = 'f') or (v = 'F') or (v = 'n') or (v = 'N'));
  end else if value <> '' then
  begin
    valueLower := value.ToLower;
    Result := not ((valueLower = 'false') or (valueLower = 'no') or (valueLower = 'off'));
  end
  else
    Result := True;
end;

function ParseBoolFlag(const str, flag: string; var value: Boolean): Boolean;
var
  valueStr: string;
begin
  // Gets the value of the flag as a string.
  valueStr := ParseFlagValue(str, flag, True);

  // Aborts if the parsing failed.
  if valueStr = '' then Exit(False);

  // Converts the string value to a bool.
  value := IsTruthyFlagValue(valueStr);
  Result := True;
end;

function ParseInt32Flag(const str, flag: string; var value: Integer): Boolean;
var
  valueStr: string;
begin
  // Gets the value of the flag as a string.
  valueStr := ParseFlagValue(str, flag, False);

  // Aborts if the parsing failed.
  if valueStr = '' then Exit(False);

  // Sets value to the value of the flag.
  Result := TryStrToInt(valueStr, value);
  if not Result then
    WriteLine('The value of flag --%s is expected to be a 32-bit integer, ' +
              'but actually has value "%s".', [flag, valueStr]);
end;

function ParseDoubleFlag(const str, flag: string; var value: Double): Boolean;
var
  valueStr: string;
begin
  // Gets the value of the flag as a string.
  valueStr := ParseFlagValue(str, flag, False);

  // Aborts if the parsing failed.
  if valueStr = '' then Exit(False);

  // Sets value to the value of the flag.
  Result := TryStrToFloat(valueStr, value);
  if not Result then
    WriteLine('The value of flag --%s is expected to be a double, ' +
              'but actually has value "%s".', [flag, valueStr]);
end;

function ParseStringFlag(const str, flag: string; var value: string): Boolean;
var
  valueStr: string;
begin
  // Gets the value of the flag as a string.
  valueStr := ParseFlagValue(str, flag, False);

  // Aborts if the parsing failed.
  if valueStr = '' then Exit(False);

  value := valueStr;
  Result := True;
end;

procedure PrintUsageAndExit;
begin
  WriteLine(
          'benchmark' + sLineBreak +
          ' [--benchmark_list_tests={true|false}]' + sLineBreak +
          '          [--benchmark_filter=<regex>]' + sLineBreak +
          '          [--benchmark_min_time=<min_time>]' + sLineBreak +
          '          [--benchmark_repetitions=<num_repetitions>]' + sLineBreak +
          '          [--benchmark_report_aggregates_only={true|false}]' + sLineBreak +
          '          [--benchmark_display_aggregates_only={true|false}]' + sLineBreak +
          '          [--benchmark_format=<console|json|csv>]' + sLineBreak +
          '          [--benchmark_out=<filename>]' + sLineBreak +
          '          [--benchmark_out_format=<json|console|csv>]' + sLineBreak +
          '          [--benchmark_color={auto|true|false}]' + sLineBreak +
          '          [--benchmark_counters_tabular={true|false}]' + sLineBreak +
          '          [--log_level=<verbosity>]');
  Halt(0);
end;

procedure ParseCommandLineFlags;
const
  allowedFormats: array[0..2] of string = ('console', 'json', 'csv');
var
  i: Integer;
  arg: string;
begin
  TBenchmarkReporter.TContext.executableName := ParamStr(0);
  for i := 1 to ParamCount do
  begin
    arg := ParamStr(i);
    if not (ParseBoolFlag(arg, 'benchmark_list_tests',
                                benchmark_list_tests) or
        ParseStringFlag(arg, 'benchmark_filter', benchmark_filter) or
        ParseDoubleFlag(arg, 'benchmark_min_time',
                              benchmark_min_time) or
        ParseInt32Flag(arg, 'benchmark_repetitions',
                             benchmark_repetitions) or
        ParseBoolFlag(arg, 'benchmark_report_aggregates_only',
                            benchmark_report_aggregates_only) or
        ParseBoolFlag(arg, 'benchmark_display_aggregates_only',
                            benchmark_display_aggregates_only) or
        ParseStringFlag(arg, 'benchmark_format', benchmark_format) or
        ParseStringFlag(arg, 'benchmark_out', benchmark_out) or
        ParseStringFlag(arg, 'benchmark_out_format',
                              benchmark_out_format) or
        ParseStringFlag(arg, 'benchmark_color', benchmark_color) or
        ParseBoolFlag(arg, 'benchmark_counters_tabular',
                            benchmark_counters_tabular) or
        ParseInt32Flag(arg, 'log_level', log_level)) then
      if IsFlag(arg, 'help') then
        PrintUsageAndExit
      else
      begin
        WriteLine('error: unrecognozed command-line flag: ' + arg);
        Halt(1);
      end;
  end;
  if not MatchText(benchmark_format, allowedFormats) or
     not MatchText(benchmark_out_format, allowedFormats) then
    PrintUsageAndExit;
  if benchmark_color = '' then
    PrintUsageAndExit;
end;

{$ENDREGION}


{$REGION 'Utilities'}

procedure DiagnoseAndExit(const msg: string);
const
  EXIT_SUCCESS = 0;
  EXIT_FAILURE = 8;
begin
  WriteLine(msg);
  Halt(EXIT_FAILURE);
end;

function IsZero(const n: Double): Boolean;
const
  DBL_EPSILON = 2.2204460492503131e-16;
begin
  Result := Abs(n) < DBL_EPSILON;
end;

{$ENDREGION}


{$REGION 'Helpers'}

function TUserCountersHelper.Find(const name: string): PCounter;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if Self[i].name = name then
      Exit(@Self[i].counter);
  Result := nil;
end;

function TUserCountersHelper.Get(const name: string): PCounter;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if Self[i].name = name then
      Exit(@Self[i].counter);
  i := Length(Self);
  SetLength(Self, i + 1);
  Self[i].name := name;
  Result := @Self[i].counter;
end;

function TCounterStatsHelper.Find(const name: string): PCounterStat;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if Self[i].name = name then
      Exit(@Self[i].counter);
  Result := nil;
end;

function TCounterStatsHelper.Get(const name: string): PCounterStat;
var
  i: Integer;
begin
  for i := 0 to High(Self) do
    if Self[i].name = name then
      Exit(@Self[i].counter);
  i := Length(Self);
  SetLength(Self, i + 1);
  Self[i].name := name;
  Result := @Self[i].counter;
end;

{$ENDREGION}


{$REGION 'String formatting'}

// ported from: benchmark.h, console_reporter.cc, string_util.cc

function GetTimeUnitString(timeUnit: TTimeUnit): string;
const
  strings: array[TTimeUnit] of string = (
    'ns', 'us', 'ms', 's');
begin
  Result := strings[timeUnit];
end;

function GetTimeUnitMultiplier(timeUnit: TTimeUnit): Double; inline;
const
  multipliers: array[TTimeUnit] of Double = (
    1000000000, 1000000, 1000, 1);
begin
  Result := multipliers[timeUnit];
end;

function FormatTime(const time: Double): string;
begin
  if time < 1.0 then
    Result := Format('%10.3f', [time])
  else if time < 10.0 then
    Result := Format('%10.2f', [time])
  else if time < 100.0 then
    Result := Format('%10.1f', [time])
  else
    Result := Format('%10.0f', [time]);
end;

function LocalDataTimeString: string;
begin
  Result := DateTimeToStr(Now);
end;

const
  // kilo, Mega, Giga, Tera, Peta, Exa, Zetta, Yotta.
  kBigSIUnits: array[0..7] of AnsiChar = 'kMGTPEZY';
  // Kibi, Mebi, Gibi, Tebi, Pebi, Exbi, Zebi, Yobi.
  kBigIECUnits: array[0..7] of AnsiChar = 'KMGTPEZY';
  // milli, micro, nano, pico, femto, atto, zepto, yocto.
  kSmallSIUnits: array[0..7] of AnsiChar = 'munpfazy';

  kUnitsSize = SizeOf(kBigSIUnits);

procedure ToExponentAndMantissa(val, thresh: Double; precision: Integer;
                                oneK: Double; var mantissa: string;
                                var exponent: Int64);

const
  fmt: TFormatSettings = (DecimalSeparator: '.');
  outputPrecision = 6;
var
  adjusted_threshold,
  big_threshold,
  small_threshold,
  simple_threshold,
  scaled: Double;
  i: Integer;
begin
  if val < 0 then
  begin
    mantissa := '-';
    val := -val;
  end;

  // Adjust threshold so that it never excludes things which can't be rendered
  // in 'precision' digits.
  adjusted_threshold := Max(thresh, 1.0 / Power(10.0, precision));
  big_threshold := adjusted_threshold * oneK;
  small_threshold := adjusted_threshold;
  // Values in ]simple_threshold,small_threshold[ will be printed as-is
  simple_threshold := 0.01;

  if val > big_threshold then
  begin
    // Positive powers
    scaled := val;
    for i := 0 to High(kBigSIUnits) do
    begin
      scaled := scaled / oneK;
      if scaled <= big_threshold then
      begin
        mantissa := mantissa + scaled.ToString(ffGeneral, outputPrecision, 0, fmt);
        exponent := i + 1;
        Exit;
      end;
    end;
    mantissa := mantissa + val.ToString(ffGeneral, outputPrecision, 0, fmt);
    exponent := 0;
  end
  else if val < small_threshold then
  begin
    // Negative powers
    if val < simple_threshold then
    begin
      scaled := val;
      for i := 0 to High(kSmallSIUnits) do
      begin
        scaled := scaled * oneK;
        if scaled >= small_threshold then
        begin
          mantissa := mantissa + scaled.ToString(ffGeneral, outputPrecision, 0, fmt);
          exponent := -(i + 1);
          Exit;
        end;
      end;
    end;
    mantissa := mantissa + val.ToString(ffGeneral, outputPrecision, 0, fmt);
    exponent := 0;
  end
  else
  begin
    mantissa := mantissa + val.ToString(ffGeneral, outputPrecision, 0, fmt);
    exponent := 0;
  end;
end;

function ExponentToPrefix(exponent: Int64; iec: Boolean): string;
var
  index: Int64;
  units: PAnsiChar;
begin
  if exponent = 0 then Exit('');

  if exponent > 0 then
    index := exponent - 1
  else
    index := -exponent - 1;
  if index >= kUnitsSize then Exit('');

  if exponent > 0 then
    if iec then
      units := kBigIECUnits
    else
      units := kBigSIUnits
  else
    units := kSmallSIUnits;
  if iec then
    Result := units[index] + 'i'
  else
    Result := string(units[index]);
end;

function ToBinaryStringFullySpecified(value, threshold: Double;
                                      precision: Integer; oneK: Double = 1024.0): string;
var
  mantissa: string;
  exponent: Int64;
begin
  ToExponentAndMantissa(value, threshold, precision, oneK, mantissa, exponent);
  Result := mantissa + ExponentToPrefix(exponent, False);
end;

function HumanReadableNumber(n, oneK: Double): string;
begin
  // 1.1 means that figures up to 1.1k should be shown with the next unit down;
  // this softens edge effects.
  // 1 means that we should show one decimal place of precision.
  Result := ToBinaryStringFullySpecified(n, 1.1, 1, oneK);
end;

function GetBigOString(const complexity: TBigO): string;
const
  complexityStrs: array[TBigO] of string = (
    '', '(1)', 'N', 'N^2', 'N^3', 'lgN', 'NlgN', '', 'f(N)'
  );
begin
  Result := complexityStrs[complexity];
end;

{$ENDREGION}


{$REGION 'Clock and timers'}

// ported from: timers.cc and timers.h

{$IFDEF DELPHI_DARWIN}
type
  clockid_t = clock_res_t;

function clock_gettime(clk_id: clockid_t; ts: Ptimespec): Integer; cdecl;
  external libc name _PU + 'clock_gettime';
{$EXTERNALSYM clock_gettime}

const
  CLOCK_REALTIME = 0;
  CLOCK_MONOTONIC_RAW = 4;
  CLOCK_MONOTONIC_RAW_APPROX = 5;
  CLOCK_MONOTONIC = 6;
  CLOCK_UPTIME_RAW = 8;
  CLOCK_UPTIME_RAW_APPROX = 9;
  CLOCK_PROCESS_CPUTIME_ID = 12;
  CLOCK_THREAD_CPUTIME_ID = 16;
{$ENDIF}

{$IFDEF MSWINDOWS}
function MakeTime(const kernelTime: TFileTime; const userTime: TFileTime): Double;
var
  kernel, user: ULARGE_INTEGER;
begin
  kernel.HighPart := kernelTime.dwHighDateTime;
  kernel.LowPart := kernelTime.dwLowDateTime;
  user.HighPart := userTime.dwHighDateTime;
  user.LowPart := userTime.dwLowDateTime;
  Result := (kernel.QuadPart + user.QuadPart) * 1e-7;
end;
{$ELSE}
function MakeTime(const ts: timespec): Double; inline;
begin
  Result := ts.tv_sec + (ts.tv_nsec * 1e-9);
end;
{$ENDIF}

function ProcessCPUUsage: Double;
{$IFDEF MSWINDOWS}
var
  proc: THandle;
  creationTime, exitTime, kernelTime, userTime: TFileTime;
begin
  proc := GetCurrentProcess;
  if GetProcessTimes(proc, creationTime, exitTime, kernelTime, userTime) then
    Exit(MakeTime(kernelTime, userTime));
  DiagnoseAndExit('GetProccessTimes() failed');
  Result := 0;
end;
{$ELSE}
var
  spec: timespec;
begin
  if clock_gettime(CLOCK_PROCESS_CPUTIME_ID, @spec) = 0 then
    Exit(MakeTime(spec));
  DiagnoseAndExit('clock_gettime(CLOCK_PROCESS_CPUTIME_ID, ...) failed');
  Result := 0;
end;
{$ENDIF}

function ThreadCPUUsage: Double;
{$IFDEF MSWINDOWS}
var
  thisThread: THandle;
  creationTime, exitTime, kernelTime, userTime: TFileTime;
begin
  thisThread := GetCurrentThread;
  if GetThreadTimes(thisThread, creationTime, exitTime, kernelTime, userTime) then
    Exit(MakeTime(kernelTime, userTime));
  DiagnoseAndExit('GetThreadTimes() failed');
  Result := 0;
end;
{$ELSE}
var
  ts: timespec;
begin
  if clock_gettime(CLOCK_THREAD_CPUTIME_ID, @ts) = 0 then
    Exit(MakeTime(ts));
  DiagnoseAndExit('clock_gettime(CLOCK_THREAD_CPUTIME_ID, ...) failed');
  Result := 0;
end;
{$ENDIF}

type
  TClock = record
    {$IFDEF MSWINDOWS}
    class var freq: Int64;
    class constructor Create;
    {$ENDIF}
    class function Now: Double; static; inline;
  end;

{$IFDEF MSWINDOWS}
class constructor TClock.Create;
begin
  QueryPerformanceFrequency(freq);
end;
{$ENDIF}

class function TClock.Now: Double;
{$IFDEF MSWINDOWS}
var
  ticks: Int64;
begin
  QueryPerformanceCounter(ticks);
  Result := ticks / freq;
end;
{$ELSE}
var
  res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := MakeTime(res);
end;
{$ENDIF}

function ChronoClockNow: Double; inline;
begin
  Result := TClock.Now;
end;

{$ENDREGION}


{$REGION 'System info'}

// ported from: sysinfo.cc

{$IFNDEF MSWINDOWS}
function GetSystemInfo(const fileName: string): string;
const
  bufferSize = 1024;
var
  f: Integer;
  buffer: array[1..bufferSize] of AnsiChar;
  bytesRead: Integer;
  s: AnsiString;
begin
  Result := '';
  f := __open(PUtf8Char(Utf8String(fileName)), O_RDONLY, 0);
  if f = -1 then
    Exit;

  repeat
    bytesRead := __read(f, @buffer[1], Length(buffer));
    if bytesRead > 0 then
    begin
      s := buffer;
      SetLength(s, bytesRead);
      Result := Result + string(s);
    end;
  until bytesRead < bufferSize;
  __close(f);
  Result := Trim(Result);
end;

function GetSystemInfoByName(const fileName: string; const name: string): string;
var
  s: string;
begin
  for s in GetSystemInfo(fileName).Split([#10, #13]) do
    if s.StartsWith(name, True) then
      Exit(Trim(Copy(s, Pos(':', s) + 1)));
  Result := '';
end;

function GetCPUFreq(const fileName: string): Double;
begin
  // frequencies are in kHz
  Result := StrToFloatDef(GetSystemInfo(fileName), 0) * 1000;
end;
{$ENDIF}

function GetNumCPUs: Integer;
{$IFDEF MSWINDOWS}
var
  sysInfo: TSystemInfo;
begin
  sysInfo := Default(TSystemInfo);
  GetSystemInfo(sysInfo);
  Result := sysInfo.dwNumberOfProcessors;
end;
{$ELSE}
begin
  Result := CPUCount;
end;
{$ENDIF}

function GetCPUCyclesPerSecond: Double;
{$IFDEF MSWINDOWS}

  function GetCycles: UInt64; {$IFDEF FPC}nostackframe; assembler;{$ENDIF}
  asm
    {$IFDEF CPUX64}
    {$IFNDEF FPC}.noframe{$ENDIF}
    rdtsc
    shl rdx,32
    or rax,rdx
    {$ELSE}
    rdtsc
    {$ENDIF}
  end;

var
  freq, stop, counter: Int64;
  cyclesStart, cyclesStop: UInt64;
const
  WaitFactor = 10; // wait 1/10 of a second
begin
  if not QueryPerformanceFrequency(freq) then Exit(0);
  QueryPerformanceCounter(stop);
  Inc(stop, freq div WaitFactor);
  cyclesStart := GetCycles;
  repeat
    QueryPerformanceCounter(counter);
  until counter >= stop;
  cyclesStop := GetCycles;
  if cyclesStop > cyclesStart then
    Result := cyclesStop - cyclesStart
  else
    Result := High(UInt64) - cyclesStart + cyclesStop;
  Result := Result * WaitFactor;
end;
{$ELSE}
{$IFDEF DELPHI_DARWIN}
var
  freq: uint64_t;
  size: size_t;
begin
  Result := 0;
  size := SizeOf(freq);
  if sysctlbyname('hw.cpufrequency', @freq, @size, nil, 0) = 0 then
    Result := freq;
end;
{$ELSE}
begin
  Result := GetCPUFreq('/sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq');
  if Result = 0 then
    Result := StrToFloatDef(GetSystemInfoByName('/proc/cpuinfo', 'CPU MHz'), 0) * 1000000;
end;
{$ENDIF}
{$ENDIF}

function GetCacheSizes: TArray<TCPUInfo.TCacheInfo>;
{$IFDEF MSWINDOWS}

  {$IFDEF FPC}
  function CountPopulation32(x: Cardinal): Integer; inline;
  begin
    Result := PopCnt(x);
  end;
  {$ELSE}
  {$IF not declared(System.CountPopulation32)}
  function CountPopulation32(x: Cardinal): Integer; inline;
  begin
    {$IFOPT R+}{$R-}{$DEFINE RANGECHECKS_ON}{$ENDIF}
    {$IFOPT Q+}{$Q-}{$DEFINE OVERFLOWCHECKS_ON}{$ENDIF}
    x := x - ((x shr 1) and $55555555);
    x := ((x shr 2) and $33333333) + (x and $33333333);
    Result := ((((x shr 4) + x) and $0F0F0F0F) * $01010101) shr 24;
    {$IFDEF OVERFLOWCHECKS_ON}{$Q+}{$ENDIF}
    {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
  end;
  {$IFEND}
  {$ENDIF}

const
  CacheTypes: array[TProcessorCacheType] of string = ('Unified', 'Instruction', 'Data', 'Trace');
var
  res: TArray<TCPUInfo.TCacheInfo>;
  bufferSize: DWORD;
  buff, it: PSystemLogicalProcessorInformation;
  i: Integer;
  cache: PCacheDescriptor;
  c: TCPUInfo.TCacheInfo;
begin
  bufferSize := 0;
  GetLogicalProcessorInformation(nil, bufferSize);
  GetMem(buff, bufferSize);
  if not GetLogicalProcessorInformation(buff, bufferSize) then
    DiagnoseAndExit('Failed during call to GetLogicalProcessorInformation: ' + GetLastError.ToString);

  for i := 0 to (bufferSize div SizeOf(TSystemLogicalProcessorInformation)) - 1 do
  begin
    {$POINTERMATH ON}
    it := @buff[i];
    {$POINTERMATH OFF}
    if it.Relationship <> RelationCache then Continue;
    // To prevent duplicates, only consider caches where CPU 0 is specified
    if not Odd(it.ProcessorMask) then Continue;
    cache := @it.Cache;
    c.numSharing := CountPopulation32(it.ProcessorMask);
    c.level := cache.Level;
    c.size := cache.Size;
    if cache._Type <= High(TProcessorCacheType) then
      c.typ := CacheTypes[cache._Type]
    else
      c.typ := 'Unknown';
    res := res + [c];
  end;
  FreeMem(buff);
  Result := res;
end;
{$ELSE}
{$IFDEF DELPHI_DARWIN}
type
  TCacheType = record
    name: AnsiString;
    typ: string;
    level: Integer;
  end;
const
  CacheTypes: array[0..3] of TCacheType = (
    (name: 'hw.l1dcachesize'; typ: 'Data'; level: 1),
    (name: 'hw.l1icachesize'; typ: 'Instruction'; level: 1),
    (name: 'hw.l2cachesize'; typ: 'Unified'; level: 2),
    (name: 'hw.l3cachesize'; typ: 'Unified'; level: 3));
var
  res: TArray<TCPUInfo.TCacheInfo>;
  size: size_t;
  cacheCounts: TArray<UInt64>;
  val: Int64;
  cache: TCacheType;
  c: TCPUInfo.TCacheInfo;
begin
  size := 0;
  if sysctlbyname('hw.cacheconfig', nil, @size, nil, 0) = 0 then
  begin
    SetLength(cacheCounts, size div 8);
    if sysctlbyname('hw.cacheconfig', @cacheCounts[0], @size, nil, 0) = 0 then
      for cache in CacheTypes do
      begin
        size := SizeOf(val);
        if sysctlbyname(PAnsiChar(cache.name), @val, @size, nil, 0) = 0 then
        begin
          c.typ := cache.typ;
          c.level := cache.level;
          c.size := val;
          c.numSharing := cacheCounts[cache.level];
          res := res + [c];
        end;
      end;
  end;
  Result := res;
end;
{$ELSE}

  function CountBits(const part: string): Integer;
  var
    i, n: Integer;
  begin
    Result := 0;
    n := Length(part);
    for i := n downto 1 do
      if part[i] = '1' then
        Result := Result + (1 shl (n - i));
  end;

  function CountSetBitsInCPUMap(value: string): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    repeat
      i := Pos(',', value);
      if i = 0 then
      begin
        Result := Result + CountBits(value);
        Break;
      end;
      Result := Result + CountBits(Copy(value, 1, i - 1));
      value := Copy(value, i + 1);
    until False;
  end;

const
  dir = '/sys/devices/system/cpu/cpu0/cache/';
var
  res: TArray<TCPUInfo.TCacheInfo>;
  path, s: string;
  idx: Integer;
  info: TCPUInfo.TCacheInfo;
begin
  idx := 0;
  repeat
    path := dir + 'index' + idx.ToString + '/';
    Inc(idx);
    s := GetSystemInfo(path + 'size');
    if s = '' then Break;

    if s.EndsWith('K') then
    begin
      info.size := StrToIntDef(Copy(s, 1, Length(s)-1), 0);
      info.size := info.size * 1024;
    end
    else
      info.size := StrToIntDef(s, 0);
    s := GetSystemInfo(path + 'type');
    info.typ := s;
    s := GetSystemInfo(path + 'level');
    info.level := StrToIntDef(s, 0);
    s := GetSystemInfo(path + 'shared_cpu_map');
    info.numSharing := CountSetBitsInCPUMap(s);

    res := res + [info];
  until False;

  Result := res;
end;
{$ENDIF}
{$ENDIF}

{$ENDREGION}


{$REGION 'Range handling'}

// ported from: benchmark_register.h

function AddPowers(var dst: TArray<Int64>; lo, hi: Int64; mult: Integer): Integer;
var
  startOffset: Integer;
  i: Int64;
begin
  Assert(lo >= 0);
  Assert(hi >= lo);
  Assert(mult >= 2);

  startOffset := Length(dst);

  // Space out the values in multiples of "mult"
  i := 1;
  while i <= hi do
  begin
    if i >= lo then
      dst := dst + [i];

    // Break the loop here since multiplying by
    // 'mult' would move outside of the range of T
    if i > High(Int64) / mult then Break;

    i := i * mult;
  end;

  Result := startOffset;
end;

procedure AddNegatedPowers(var dst: TArray<Int64>; lo, hi: Int64; mult: Integer);
var
  loComplement, hiComplement: Int64;
  i, it: Integer;
  temp: Int64;
begin
  // We negate lo and hi so we require that they cannot be equal to 'min'.
  Assert(lo > Low(Int64));
  Assert(hi > Low(Int64));
  Assert(hi > lo);
  Assert(hi < 0);

  // Add positive powers, then negate and reverse.
  // Casts necessary since small integers get promoted
  // to 'int' when negating.
  loComplement := -lo;
  hiComplement := -hi;

  it := AddPowers(dst, hiComplement, loComplement, mult);

  for i := it to High(dst) do
    dst[i] := dst[i] * -1;
  i := High(dst);
  while it < i do
  begin
    temp := dst[it];
    dst[it] := dst[i];
    dst[i] := temp;
    Inc(it);
    Dec(i);
  end;
end;

procedure AddRange(var dst: TArray<Int64>; lo, hi: Int64; mult: Integer);
var
  loInner, hiInner: Int64;
begin
  Assert(hi >= lo);
  Assert(mult >= 2);

  // Add "lo"
  dst := dst + [lo];

  // Handle lo == hi as a special case, so we then know
  // lo < hi and so it is safe to add 1 to lo and subtract 1
  // from hi without falling outside of the range.
  if lo = hi then Exit;

  // Ensure that lo_inner <= hi_inner below.
  if lo + 1 = hi then
  begin
    dst := dst + [hi];
    Exit;
  end;

  // Add all powers of 'mult' in the range [lo+1, hi-1] (inclusive).
  loInner := lo + 1;
  hiInner := hi - 1;

  // Insert negative values
  if loInner < 0 then
    AddNegatedPowers(dst, loInner, Min(hiInner, -1), mult);

  // Treat 0 as a special case (see discussion on #762).
  if (lo < 0) and (hi >= 0) then
    dst := dst + [0];

  // Insert positive values
  if hiInner > 0 then
    AddPowers(dst, Max(loInner, 1), hiInner, mult);

  // Add "hi" (if different from last value).
  if hi <> dst[High(dst)] then
    dst := dst + [hi];
end;

{$ENDREGION}


{$REGION 'Complexity calculation'}

// ported from: complexity.cc, complexity.h

function __oN(const n: TIterationCount): Double; begin Result := n; end;
function __oNSquared(const n: TIterationCount): Double; begin Result := Power(n, 2); end;
function __oNCubed(const n: TIterationCount): Double; begin Result := Power(n, 3); end;
function __oLogN(const n: TIterationCount): Double; begin Result := Log2(n); end;
function __oNLogN(const n: TIterationCount): Double; begin Result := n * Log2(n); end;
function __o1(const n: TIterationCount): Double; begin Result := 1; end;

function FittingCurve(const complexity: TBigO): TBigOFunc;
begin
  case complexity of
    oN:         Result := __oN;
    oNSquared:  Result := __oNSquared;
    oNCubed:    Result := __oNCubed;
    oLogN:      Result := __oLogN;
    oNLogN:     Result := __oNLogN;
  else
    Result := __o1;
  end;
end;

// This data structure will contain the result returned by MinimalLeastSq
//   - coef        : Estimated coeficient for the high-order term as
//                   interpolated from data.
//   - rms         : Normalized Root Mean Squared Error.
//   - complexity  : Scalability form (e.g. oN, oNLogN). In case a scalability
//                   form has been provided to MinimalLeastSq this will return
//                   the same value. In case TBigO.oAuto has been selected, this
//                   parameter will return the best fitting curve detected.
type
  TLeastSq = record
    coef: Double;
    rms: Double;
    complexity: TBigO;
  end;

// Find the coefficient for the high-order term in the running time, by
// minimizing the sum of squares of relative error, for the fitting curve
// given by the lambda expression.
//   - n             : Vector containing the size of the benchmark tests.
//   - time          : Vector containing the times for the benchmark tests.
//   - fitting_curve : lambda expression (e.g. [](int64_t n) {return n; };).

// For a deeper explanation on the algorithm logic, please refer to
// https://en.wikipedia.org/wiki/Least_squares#Least_squares,_regression_analysis_and_statistics
function MinimalLeastSq(const n: TArray<Int64>; const time: TArray<Double>;
  const fittingCurve: TBigOFunc): TLeastSq; overload;
var
  sigma_gn, sigma_gn_squared, sigma_time, sigma_time_gn,
  gn_i, rms, fit, mean: Double;
  i: Integer;
begin
  sigma_gn := 0.0;
  sigma_gn_squared := 0.0;
  sigma_time := 0.0;
  sigma_time_gn := 0.0;

  // Calculate least square fitting parameter
  for i := 0 to High(n) do
  begin
    gn_i := fittingCurve(n[i]);
    sigma_gn := sigma_gn + gn_i;
    sigma_gn_squared := sigma_gn_squared + gn_i * gn_i;
    sigma_time := sigma_time + time[i];
    sigma_time_gn := sigma_time_gn + time[i] * gn_i;
  end;

  Result.complexity := oLambda;

  // Calculate complexity.
  Result.coef := sigma_time_gn / sigma_gn_squared;

  // Calculate RMS
  rms := 0.0;
  for i := 0 to High(n) do
  begin
    fit := Result.coef * fittingCurve(n[i]);
    rms := rms + Power(time[i] - fit, 2);
  end;

  // Normalized RMS by the mean of the observed values
  mean := sigma_time / Length(n);
  Result.rms := Sqrt(rms / Length(n)) / mean;
end;

// Find the coefficient for the high-order term in the running time, by
// minimizing the sum of squares of relative error.
//   - n          : Vector containing the size of the benchmark tests.
//   - time       : Vector containing the times for the benchmark tests.
//   - complexity : If different than oAuto, the fitting curve will stick to
//                  this one. If it is oAuto, it will be calculated the best
//                  fitting curve.
function MinimalLeastSq(const n: TArray<Int64>; const time: TArray<Double>;
  const complexity: TBigO): TLeastSq; overload;
const
  fitCurves = [oLogN, oN, oNLogN, oNSquared, oNCubed];
var
  bestFit: TLeastSq;
  fit: TBigO;
  currentFit: TLeastSq;
begin
  Assert(Length(n) = Length(time));
  Assert(Length(n) >= 2); // Do not compute fitting curve is less than two
                          // benchmark runs are given
  Assert(complexity <> oNone);

  if complexity = oAuto then
  begin
    // Take o1 as default best fitting curve
    bestFit := MinimalLeastSq(n, time, FittingCurve(o1));
    bestFit.complexity := o1;

    // Compute all possible fitting curves and stick to the best one
    for fit in fitCurves do
    begin
      currentFit := MinimalLeastSq(n, time, FittingCurve(fit));
      if currentFit.rms < bestFit.rms then
      begin
        bestFit := currentFit;
        bestFit.complexity := fit;
      end;
    end;
  end else begin
    bestFit := MinimalLeastSq(n, time, FittingCurve(complexity));
    bestFit.complexity := complexity;
  end;

  Result := bestFit;
end;

function ComputeBigO(const reports: TArray<TBenchmarkReporter.TRun>): TArray<TBenchmarkReporter.TRun>;
type
  TRun = TBenchmarkReporter.TRun;
var
  results: TArray<TRun>;
  n: TArray<Int64>;
  realTime, cpuTime: TArray<Double>;
  i: Integer;
  resultCpu, resultReal: TLeastSq;
  runName: TBenchmarkName;
  bigO, rms: TRun;
  multiplier: Double;
begin
  if Length(reports) < 2 then Exit;

  // Accumulators.
  SetLength(n, Length(reports));
  SetLength(realTime, Length(reports));
  SetLength(cpuTime, Length(reports));

  // Populate the accumulators
  for i := 0 to High(reports) do
  begin
    Assert(reports[i].complexityN > 0, 'Did you forget to call SetComplexityN?');
    n[i] := reports[i].complexityN;
    realTime[i] := reports[i].realAccumulatedTime / reports[i].iterations;
    cpuTime[i] := reports[i].cpuAccumulatedTime / reports[i].iterations;
  end;

  if reports[0].complexity = oLambda then
  begin
    resultCpu := MinimalLeastSq(n, cpuTime, reports[0].complexityLambda);
    resultReal := MinimalLeastSq(n, realTime, reports[0].complexityLambda);
  end else begin
    resultCpu := MinimalLeastSq(n, cpuTime, reports[0].complexity);
    resultReal := MinimalLeastSq(n, realTime, resultCpu.complexity);
  end;

  // Drop the 'args' when reporting complexity.
  runName := reports[0].runName;
  runName.args := '';

  // Get the data from the accumulator to TBenchmarkReporter.TRun's.
  bigO := TRun.Create;
  bigO.runName := runName;
  bigO.runtype := TRun.TRunType.rtAggregate;
  bigO.repetitions := reports[0].repetitions;
  bigO.repetitionIndex := TRun.noRepetitionIndex;
  bigO.threads := reports[0].threads;
  bigO.aggregateName := 'BigO';
  bigO.reportLabel := reports[0].reportLabel;
  bigO.iterations := 0;
  bigO.realAccumulatedTime := resultReal.coef;
  bigO.cpuAccumulatedTime := resultCpu.coef;
  bigO.reportBigO := True;
  bigO.complexity := resultCpu.complexity;

  // All the time results are reported after being multiplied by the
  // time unit multiplier. But since RMS is a relative quantity it
  // should not be multiplied at all. So, here, we _divide_ it by the
  // multiplier so that when it is multiplied later the result is the
  // correct one.
  multiplier := GetTimeUnitMultiplier(reports[0].timeUnit);

  // Only add label to mean/stddev if it is same for all runs
  rms := TRun.Create;
  rms.runName := runName;
  rms.runType := TRun.TRunType.rtAggregate;
  rms.aggregateName := 'RMS';
  rms.reportLabel := bigO.reportLabel;
  rms.iterations := 0;
  rms.repetitionIndex := TRun.noRepetitionIndex;
  rms.repetitions := reports[0].repetitions;
  rms.threads := reports[0].threads;
  rms.realAccumulatedTime := resultReal.rms / multiplier;
  rms.cpuAccumulatedTime := resultCpu.rms / multiplier;
  rms.reportRms := True;
  rms.complexity := resultCpu.complexity;
  // don't forget to keep the time unit, or we won't be able to
  // recover the correct value.
  rms.timeUnit := reports[0].timeUnit;

  results := results + [bigO, rms];
  Result := results;
end;

{$ENDREGION}


{$REGION 'Counters'}

// ported from: counter.cc

function Finish(var counter: TCounter; const iterations: TIterationCount;
  const cpuTime, numThreads: Double): Double; overload;
var
  v: Double;
begin
  v := counter.value;
  if kIsRate in counter.flags then
    v := v / cpuTime;
  if kAvgThreads in counter.flags then
    v := v / numThreads;
  if kIsIterationInvariant in counter.flags then
    v := v * iterations;
  if kAvgIterations in counter.flags then
    v := v / iterations;

  if kInvert in counter.flags then  // Invert is *always* last.
    v := 1 / v;

  Result := v;
end;

procedure Finish(var counters: TUserCounters; const iterations: TIterationCount;
  const cpuTime, numThreads: Double); overload;
var
  i: Integer;
begin
  for i := 0 to High(counters) do
     counters[i].counter.value := Finish(counters[i].counter, iterations, cpuTime, numThreads);
end;

procedure Increment(var left: TUserCounters; const right: TUserCounters);
var
  i: Integer;
  it: PCounter;
begin
  // add counters present in both or just in left
  for i := 0 to High(left) do
  begin
    it := right.Find(left[i].Name);
    if it <> nil then
      left[i].Counter.value := left[i].Counter.value + it^.value;
  end;
  // add counters present in r, but not in *l
  for i := 0 to High(right) do
  begin
    it := left.Find(right[i].Name);
    if it = nil then
      left.Get(right[i].Name)^ := right[i].Counter;
  end;
end;

function SameNames(const left, right: TUserCounters): Boolean;
var
  i: Integer;
begin
  if left = right then Exit(True);
  if Length(left) <> LEngth(right) then Exit(False);
  for i := 0 to High(left) do
    if right.Find(left[i].name) = nil then
      Exit(False);
  Result := True;
end;

{$ENDREGION}


{$REGION 'Statistics'}

// ported from: statistics.cc

function StatisticsSum(const values: array of Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(values) do
    Result := Result + values[i];
end;

function StatisticsMean(const values: array of Double): Double;
begin
  if Length(values) = 0 then Exit(0);
  Result := StatisticsSum(values) * (1.0 / Length(values));
end;

function StatisticsMedian(const values: array of Double): Double;

   procedure Sort(var values: array of Double);
   type
     TSlice = array[0..0] of Double;
   var
     lo, hi: Integer;
     pivot, temp: Double;
   begin
     lo := 0;
     hi := High(values);
     pivot := values[(lo + hi) div 2];
     repeat
       while values[lo] < pivot do Inc(Lo);
       while values[hi] > pivot do Dec(Hi);
       if lo <= hi then
       begin
         temp := values[lo];
         values[lo] := values[hi];
         values[hi] := temp;
         Inc(lo);
         Dec(hi);
       end;
     until lo > hi;
     if hi > 0 then Sort(Slice(values, hi+1));
     {$IFOPT R+}{$R-}{$DEFINE RANGECHECKS_ON}{$ENDIF}
     if lo < High(values) then Sort(Slice(TSlice(values[lo]), High(values)-lo+1));
     {$IFDEF RANGECHECKS_ON}{$R+}{$ENDIF}
   end;

var
  copy: TArray<Double>;
  center: Integer;
begin
  if Length(values) < 3 then Exit(StatisticsMean(values));
  SetLength(copy, Length(values));
  Move(values[0], copy[0], Length(values) * SizeOf(Double));

  center := Length(values) div 2;
  // sglienke: I was too lazy to reimplement std::nth_element so let's just sort
  Sort(copy);

  // did we have an odd number of samples?
  // if yes, then center is the median
  // if no, then we are looking for the average between center and the value
  // before
  if Length(values) mod 2 = 1 then Exit(copy[center]);
  Result := (copy[center-1] + copy[center]) / 2;
end;

// Return the sum of the squares of this sample set
function SumSquares(const values: array of Double): Double;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(values) do
    Result := Result + values[i] * values[i];
end;

function Sqr(const dat: Double): Double;
begin
  Result := dat * dat;
end;

function Sqrt(const dat: Double): Double;
begin
  // Avoid NaN due to imprecision in the calculations
  if dat < 0.0 then Exit(0.0);
  Result := System.Sqrt(dat);
end;

function StatisticsStdDev(const values: array of Double): Double;
var
  mean: Double;
  avgSquares: Double;
begin
  mean := StatisticsMean(values);
  if Length(values) = 0 then Exit(mean);

  // Sample standard deviation is undefined for n = 1
  if Length(values) = 1 then Exit(0);

  avgSquares := SumSquares(values) * (1.0 / Length(values));
  Result := Sqrt(Length(values) / (Length(values) - 1.0) * (avgSquares - Sqr(mean)));
end;

function ComputeStats(const reports: array of TBenchmarkReporter.TRun): TArray<TBenchmarkReporter.TRun>;
type
  TRun = TBenchmarkReporter.TRun;
var
  results: TArray<TRun>;
  errorCount: Integer;
  i, k: Integer;
  realAccumulatedTimeStat, cpuAccumulatedTimeStat: TArray<Double>;
  realAccumulatedTimeStatIdx, cpuAccumulatedTimeStatIdx: Integer;
  runIterations: TIterationCount;
  counterStats: TCounterStats;
  it: PCounterStat;
  reportLabel: string;
  iterationRescaleFactor: Double;
  stat: ^TStatistics;
  data: TRun;
  c: PCounter;
begin
  errorCount := 0;
  for i := 0 to High(reports) do
    Inc(errorCount, Byte(reports[i].errorOccurred));

  if High(reports) - errorCount < 1 then
    // We don't report aggregated data if there was a single run.
    Exit(results);

  // Accumulators.
  SetLength(realAccumulatedTimeStat, Length(reports));
  SetLength(cpuAccumulatedTimeStat, Length(reports));
  realAccumulatedTimeStatIdx := 0;
  cpuAccumulatedTimeStatIdx := 0;

  // All repetitions should be run with the same number of iterations so we
  // can take this information from the first benchmark.
  runIterations := reports[0].iterations;

  for i := 0 to High(reports) do
  begin
    for k := 0 to High(reports[0].counters) do
    begin
      it := counterStats.Find(reports[0].counters[k].name);
      if it = nil then
      begin
        it := counterStats.Get(reports[0].counters[k].name);
        it.c := reports[0].counters[k].counter;
        SetLength(it.s, Length(reports));
      end
      else
        Assert(it^.c.flags = reports[0].counters[k].counter.flags);
    end;
  end;

  // Populate the accumulators.
  for i := 0 to High(reports) do
  begin
    Assert(reports[0].BenchmarkName = reports[i].BenchmarkName);
    Assert(runIterations = reports[i].iterations);
    if reports[i].errorOccurred then Continue;
    realAccumulatedTimeStat[realAccumulatedTimeStatIdx] := reports[i].realAccumulatedTime;
    Inc(realAccumulatedTimeStatIdx);
    cpuAccumulatedTimeStat[cpuAccumulatedTimeStatIdx] := reports[i].cpuAccumulatedTime;
    Inc(cpuAccumulatedTimeStatIdx);
    // user counters
    for k := 0 to High(reports[i].counters) do
    begin
      it := counterStats.Find(reports[i].counters[k].name);
      Assert(it <> nil);
      it.s[i] := reports[i].counters[k].counter.value;
    end;
  end;

  // Only add label if it is same for all runs
  reportLabel := reports[0].reportLabel;
  for i := 1 to High(reports) do
    if reports[i].reportLabel <> reportLabel then
    begin
      reportLabel := '';
      Break;
    end;

  iterationRescaleFactor := Length(reports) / runIterations;

  for i := 0 to High(reports[0].statistics) do
  begin
    stat := @reports[0].statistics[i];
    data := TRun.Create;
    data.runName := reports[0].runName;
    data.runType := TRun.TRunType.rtAggregate;
    data.threads := reports[0].threads;
    data.repetitions := reports[0].repetitions;
    data.repetitionIndex := TRun.noRepetitionIndex;
    data.aggregateName := stat.Name;
    data.reportLabel := reportLabel;

    // It is incorrect to say that an aggregate is computed over
    // run's iterations, because those iterations already got averaged.
    // Similarly, if there are N repetitions with 1 iterations each,
    // an aggregate will be computed over N measurements, not 1.
    // Thus it is best to simply use the count of separate reports.
    data.iterations := Length(reports);

    data.realAccumulatedTime := stat.Compute(realAccumulatedTimeStat);
    data.cpuAccumulatedTime := stat.Compute(cpuAccumulatedTimeStat);

    // We will divide these times by data.iterations when reporting, but the
    // data.iterations is not nessesairly the scale of these measurements,
    // because in each repetition, these timers are sum over all the iterations.
    // And if we want to say that the stats are over N repetitions and not
    // M iterations, we need to multiply these by (N/M).
    data.realAccumulatedTime := data.realAccumulatedTime * iterationRescaleFactor;
    data.cpuAccumulatedTime := data.cpuAccumulatedTime * iterationRescaleFactor;

    data.timeUnit := reports[0].timeUnit;

    // user counters
    for k := 0 to High(counterStats) do
    begin
      // Do *NOT* rescale the custom counters. They are already properly scaled.
      c := data.counters.Get(counterStats[k].name);
      c.value := stat.Compute(counterStats[k].counter.s);
      with counterStats.Find(counterStats[k].name).c do
      begin
        c.flags := flags;
        c.oneK := oneK;
      end;
    end;

    results := results + [data];
  end;
  Result := results;
end;

{$ENDREGION}


{$REGION 'Global routines'}

// ported from: benchmark.cc, benchmark.h, benchmark_runner.cc, benchmark_register.cc

function Range(const start, limit: Int64): TBenchmark.TRange;
begin
  Result.start := start;
  Result.limit := limit;
end;

function CreateReporter(const name: string;
  const opts: TConsoleReporter.TOutputOptions): TBenchmarkReporter;
begin
  if name = 'console' then
    Result := TConsoleReporter.Create(opts)
  else if name = 'json' then
    Result := TJSONReporter.Create
  else if name = 'csv' then
    Result := TCSVReporter.Create
  else
  begin
    WriteLine('Unexpected format: "%s"', [name]);
    Halt(1);
    Result := nil;
  end;
end;

function IsColorTerminal: Boolean;
{$IFDEF MSWINDOWS}
begin
//  // On Windows the TERM variable is usually not set, but the
//  // console there does support colors.
//  return 0 != _isatty(_fileno(stdout));
  Result := True;
end;
{$ELSE}
const
  // On non-Windows platforms, we rely on the TERM variable. This list of
  // supported TERM values is copied from Google Test:
  // <https://github.com/google/googletest/blob/master/googletest/src/gtest.cc#L2925>.
  SupportedTermValues: array[0..10] of string = (
    'xterm',         'xterm-color',     'xterm-256color',
    'screen',        'screen-256color', 'tmux',
    'tmux-256color', 'rxvt-unicode',    'rxvt-unicode-256color',
    'linux',         'cygwin'
  );
var
  term: string;
  i: Integer;
begin
  term := string(getenv('TERM'));

  Result := False;
  for i := 0 to High(SupportedTermValues) do
    if term = SupportedTermValues[i] then
    begin
      Result := True;
      Break;
    end;

  Result := Result and (isatty(STDOUT_FILENO) <> 0);
end;
{$ENDIF}

function GetOutputOptions(const forceNoColor: Boolean = False): TConsoleReporter.TOutputOptions;

  function IsBenchmarkColor: Boolean;
  begin
    if forceNoColor then
      Result := False
    else if benchmark_color = 'auto' then
      Result := IsColorTerminal
    else
      Result := IsTruthyFlagValue(benchmark_color);
  end;

begin
  Result := TConsoleReporter.ooDefaults;
  if IsBenchmarkColor then
    Include(Result, TConsoleReporter.TOutputOption.ooColor)
  else
    Exclude(Result, TConsoleReporter.TOutputOption.ooColor);
  if benchmark_counters_tabular then
    Include(Result, TConsoleReporter.TOutputOption.ooTabular)
  else
    Exclude(Result, TConsoleReporter.TOutputOption.ooTabular);
end;

function RunBenchmark(const benchmark: TBenchmarkInstance;
  var complexityReports: TArray<TBenchmarkReporter.TRun>): TBenchmarkRunner.TRunResults;
var
  r: TBenchmarkRunner;
begin
  r := TBenchmarkRunner.Create(benchmark, complexityReports);
  Result := r.GetResults;
end;

procedure RunBenchmarks(const benchmarks: array of TBenchmarkInstance;
  const displayReporter, fileReporter: TBenchmarkReporter);
var
  i, k: Integer;
  nameFieldWidth, statFieldWidth: Integer;
  context: TBenchmarkReporter.TContext;
  runResults: TBenchmarkRunner.TRunResults;
  mightHaveAggregates: Boolean;
  complexityReports: TArray<TBenchmarkReporter.TRun>;

  procedure Report(const reporter: TBenchmarkReporter;
    reportAggregatesOnly: Boolean);
  begin
    Assert(reporter <> nil);
    // If there are no aggregates, do output non-aggregates.
    reportAggregatesOnly := reportAggregatesOnly and (runResults.aggregatesOnly <> nil);
    if not reportAggregatesOnly then
      reporter.ReportRuns(runResults.nonAggregates);
    if runResults.aggregatesOnly <> nil then
      reporter.ReportRuns(runResults.aggregatesOnly);
  end;

begin
  Assert(displayReporter <> nil);

  // Determine the width of the name field using a minimum width of 10.
  mightHaveAggregates := benchmark_repetitions > 1;
  nameFieldWidth := 10;
  statFieldWidth := 0;
  for i := 0 to High(benchmarks) do
  begin
    nameFieldWidth := Max(nameFieldWidth, Length(benchmarks[i].name.Str));
    mightHaveAggregates := mightHaveAggregates or (benchmarks[i].repetitions > 1);

    for k := 0 to High(benchmarks[i].statistics) do
      statFieldWidth := Max(statFieldWidth, Length(benchmarks[i].statistics[k].Name));
  end;

  if mightHaveAggregates then
    Inc(nameFieldWidth, 1 + statFieldWidth);

  // Print header here
//  BenchmarkReporter::Context context;
  context.nameFieldWidth := nameFieldWidth;

//  // Keep track of running times of all instances of current benchmark
//  std::vector<BenchmarkReporter::Run> complexity_reports;
//
//  // We flush streams after invoking reporter methods that write to them. This
//  // ensures users get timely updates even when streams are not line-buffered.
//  auto flushStreams = [](BenchmarkReporter* reporter) {
//    if (!reporter) return;
//    std::flush(reporter->GetOutputStream());
//    std::flush(reporter->GetErrorStream());
//  };

  if displayReporter.ReportContext(context) and
    ((fileReporter = nil) or fileReporter.ReportContext(context)) then
  begin
    for i := 0 to High(benchmarks) do
    begin
      runResults := RunBenchmark(benchmarks[i], complexityReports);

      Report(displayReporter, runResults.displayReportAggregatesOnly);
      if fileReporter <> nil then
        Report(fileReporter, runResults.fileReportAggregatesOnly);
    end;
  end;

  displayReporter.Finalize;
  if Assigned(fileReporter) then
    fileReporter.Finalize;
//  flushStreams(display_reporter);
//  flushStreams(file_reporter);
end;

function RunSpecifiedBenchmarks(displayReporter,
  fileReporter: TBenchmarkReporter): Cardinal; overload;
var
  spec: string;
  benchmarks: TArray<TBenchmarkInstance>;
  defaultDisplayReporter, defaultFileReporter: TBenchmarkReporter;
  fileName: string;
  outputFile: TStream;
  i: Integer;
begin
  spec := benchmark_filter;
  if (spec = '') or (spec = 'all') then
    spec := '.';  // Regexp that matches all benchmarks

  // Setup the reporters
  defaultDisplayReporter := nil;
  defaultFileReporter := nil;

  if displayReporter = nil then
  begin
    defaultDisplayReporter := CreateReporter(benchmark_format, GetOutputOptions());
    displayReporter := defaultDisplayReporter;
  end;

  fileName := benchmark_out;
  if (fileName = '') and (fileReporter <> nil) then
  begin
    WriteLine('A custom file reporter was provided but ' +
              '--benchmark_out=<file> was not specified.');
    Halt(1);
  end;

  if fileName <> '' then
  begin
    outputFile := TFileStream.Create(fileName, fmCreate, fmShareDenyWrite);
//    if (!output_file.is_open()) {
//      Err << "invalid file name: '" << fname << "'" << std::endl;
//      std::exit(1);
//    }
    if fileReporter = nil then
    begin
      defaultFileReporter := CreateReporter(benchmark_out_format, []);
      fileReporter := defaultFileReporter;
    end;

    fileReporter.OutputStream := outputFile;
  end
  else
    outputFile := nil;

  if not TBenchmarkFamilies.FindBenchmarks(spec, benchmarks) then Exit(0);

  if benchmarks = nil then
  begin
    WriteLine('Failed to match any benchmarks against regex: ' + spec);
    Exit(0);
  end;

  if benchmark_list_tests then
    for i := 0 to High(benchmarks) do
      WriteLine(benchmarks[i].name.str)
  else
    RunBenchmarks(benchmarks, displayReporter, fileReporter);

  defaultDisplayReporter.Free;
  defaultFileReporter.Free;
  outputFile.Free;

  Result := Length(benchmarks);
end;

function RunSpecifiedBenchmarks(const displayReporter: TBenchmarkReporter): Cardinal; overload;
begin
  Result := RunSpecifiedBenchmarks(displayReporter, nil);
end;

function RunSpecifiedBenchmarks: Cardinal; overload;
begin
  Result := RunSpecifiedBenchmarks(nil, nil);
end;

procedure Initialize;
begin
  ParseCommandLineFlags;
end;

procedure Benchmark_Main(pinThread0: Boolean);
begin
  Initialize;

  {$IFDEF MSWINDOWS}
  if pinThread0 then
  begin
    SetThreadAffinityMask(GetCurrentThread, 1 shl (CPUCount - 1));
    SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_HIGHEST);
  end;
  {$ENDIF}

  RunSpecifiedBenchmarks;
end;

function RegisterBenchmarkInternal(const benchmark: TBenchmark): TBenchmark; inline;
begin
  TBenchmarkFamilies.AddBenchmark(benchmark);
  Result := benchmark;
end;

function RegisterBenchmark(const name: string; const fn: TFunction): TBenchmark; inline;
begin
  Result := RegisterBenchmarkInternal(TFunctionBenchmark.Create(name, fn));
end;

function Benchmark(const fn: TFunction; const name: string): TBenchmark;
begin
  Result := RegisterBenchmarkInternal(TFunctionBenchmark.Create(name, fn));
end;

procedure RunInThread(const benchmark: TBenchmarkInstance; iters: TIterationCount;
  threadId: Integer; const manager: TThreadManager);
var
  timer: TThreadTimer;
  st: TState;
  results: ^TThreadManager.TResult;
begin
  timer := TThreadTimer.Create(benchmark.measureProcessCpuTime);
  try
    st := benchmark.Run(iters, threadId, timer, manager);
    Assert(st.ErrorOccurred or (st.Iterations >= st.MaxIterations),
      'Benchmark returned before TState.KeepRunning() returned false!');

    manager.Lock;
    try
      results := @manager.results;
      results.iterations := results.iterations + st.iterations;
      results.cpuTimeUsed := results.cpuTimeUsed + timer.CpuTimeUsed;
      results.realTimeUsed := results.realTimeUsed + timer.RealTimeUsed;
      results.manualTimeUsed := results.manualTimeUsed + timer.ManualTimeUsed;
      results.complexityN := results.complexityN + st.ComplexityN;
      Increment(results.counters, st.fCounters);
    finally
      manager.Unlock;
      manager.NotifyThreadComplete;
    end;
  finally
    timer.Free;
  end;
end;

function CreateRunReport(const benchmark: TBenchmarkInstance;
  const results: TThreadManager.TResult;
  const memoryIterations: TIterationCount;
//  const memoryResult: TMemoryManager.TResult;
  const seconds: Double;
  const repetitionIndex: Int64
): TBenchmarkReporter.TRun;
var
  report: TBenchmarkReporter.TRun;
begin
  report := TBenchmarkReporter.TRun.Create;
  report.runName := benchmark.name;
  report.errorOccurred := results.hasError;
  report.errorMessage := results.errorMessage;
  report.reportLabel := results.reportLabel;
  // This is the total iterations across all threads.
  report.iterations := results.iterations;
  report.timeUnit := benchmark.timeUnit;
  report.threads := benchmark.threads;
  report.repetitionIndex := repetitionIndex;
  report.repetitions := benchmark.repetitions;

  if not report.errorOccurred then
  begin
    if benchmark.useManualTime then
      report.realAccumulatedTime := results.manualTimeUsed
    else
      report.realAccumulatedTime := results.realTimeUsed;
    report.cpuAccumulatedTime := results.cpuTimeUsed;
    report.complexityN := results.complexityN;
    report.complexity := benchmark.complexity;
    report.complexityLambda := benchmark.complexityLambda;
    report.statistics := benchmark.statistics;
    report.counters := results.counters;

//    if memoryIterations > 0 then
//    begin
//      report.hasMemoryResult := True;
//      report.allocsPerIter := memoryResult.numAllocs / memoryIterations;
//      report.maxBytesUsed := memoryResult.maxBytesUsed;
//    end;

    Finish(report.counters, results.iterations, seconds, benchmark.threads);
  end;
  Result := report;
end;

{$ENDREGION}


{$REGION 'TCounter' }

procedure TCounter.Init(v: Double; f: TFlags; k: TOneK);
begin
  Self.value := v;
  Self.flags := f;
  Self.oneK := k;
end;

function Counter(const value: Double; flags: TCounter.TFlags; k: TCounter.TOneK): TCounter;
begin
  Result.Init(value, flags, k);
end;

class operator TCounter.Implicit(const value: Double): TCounter;
begin
  Result.Init(value);
end;

{$ENDREGION}


{$REGION 'TStatistics'}

constructor TStatistics.Create(const name: string;
  const compute: TStatisticsFunc);
begin
  Self.Name := name;
  Self.Compute := compute;
end;

{$ENDREGION}


{$REGION 'TThreadTimer'}

constructor TThreadTimer.Create(measureProcessCpuTime: Boolean);
begin
  fMeasureProcessCpuTime := measureProcessCpuTime;
end;

constructor TThreadTimer.CreateProcessCpuTime;
begin
  fMeasureProcessCpuTime := True;
end;

function TThreadTimer.GetRealTimeRunning: Double;
begin
  Assert(not fRunning);
  Result := fRealTimeUsed;
end;

function TThreadTimer.GetCpuTimeUsed: Double;
begin
  Assert(not fRunning);
  Result := fCpuTimeUsed;
end;

function TThreadTimer.GetManualTimeUsed: Double;
begin
  Assert(not fRunning);
  Result := fManualTimeUsed;
end;

function TThreadTimer.ReadCpuTimerOfChoice: Double;
begin
  if fMeasureProcessCpuTime then
    Result := ProcessCPUUsage
  else
    Result := ThreadCPUUsage;
end;

procedure TThreadTimer.SetIterationTime(seconds: Double);
begin
  fManualTimeUsed := fManualTimeUsed + seconds;
end;

procedure TThreadTimer.StartTimer;
begin
  fRunning := True;
  fStartRealTime := ChronoClockNow;
  fStartCpuTime := ReadCpuTimerOfChoice;
end;

procedure TThreadTimer.StopTimer;
begin
  Assert(fRunning);
  fRunning := False;
  fRealTimeUsed := fRealTimeUsed + ChronoClockNow - fStartRealTime;
  fCpuTimeUsed := fCpuTimeUsed + ReadCpuTimerOfChoice - fStartCpuTime;
end;

{$ENDREGION}


{$REGION 'TBarrier'}

constructor TBarrier.Create(numThreads: Integer);
begin
  fLock := TCriticalSection.Create;
  fPhaseCondition := TConditionVariable.Create;
  fRunningThreads := numThreads;
end;

destructor TBarrier.Destroy;
begin
  fPhaseCondition.Free;
  fLock.Free;
end;

function TBarrier.Wait: Boolean;
var
  lastThread: Boolean;
begin
  fLock.Acquire;
  try
    lastThread := CreateBarrier;
  finally
    fLock.Release;
  end;
  if lastThread then
    fPhaseCondition.ReleaseAll;
  Result := lastThread;
end;

procedure TBarrier.RemoveThread;
begin
  fLock.Acquire;
  try
    Dec(fRunningThreads);
    if fEntered <> 0 then
      fPhaseCondition.ReleaseAll;
  finally
    fLock.Release;
  end;
end;

function TBarrier.CreateBarrier: Boolean;
var
  phaseNumber: Integer;
begin
  Inc(fEntered);
  if fEntered < fRunningThreads then
  begin
    // Wait for all threads to enter
    phaseNumber := fPhaseNumber;
    while not ((fPhaseNumber > phaseNumber) or (fEntered = fRunningThreads)) do
      fPhaseCondition.WaitFor(fLock);
    if fPhaseNumber > phaseNumber then
      Exit(False);
  end;
  Inc(fPhaseNumber);
  fEntered := 0;
  Result := True;
end;

{$ENDREGION}


{$REGION 'TThreadManager'}

constructor TThreadManager.Create(const numThreads: Integer);
begin
  results := Default(TResult);
  fBenchmarkLock := TCriticalSection.Create;
  fAliveThreads := numThreads;
  fStartStopBarrier := TBarrier.Create(numThreads);
  fEndCondLock := TCriticalSection.Create;
  fEndCondition := TConditionVariable.Create;
end;

destructor TThreadManager.Destroy;
begin
  fEndCondition.Free;
  fEndCondLock.Free;
  fStartStopBarrier.Free;
  fBenchmarkLock.Free;
  inherited;
end;

procedure TThreadManager.Lock;
begin
  fBenchmarkLock.Acquire;
end;

procedure TThreadManager.Unlock;
begin
  fBenchmarkLock.Release;
end;

function TThreadManager.StartStopBarrier: Boolean;
begin
  Result := fStartStopBarrier.Wait;
end;

procedure TThreadManager.NotifyThreadComplete;
begin
  fStartStopBarrier.RemoveThread;
  if AtomicDecrement(fAliveThreads) = 0 then
  begin
    fEndCondLock.Acquire;
    try
      fEndCondition.ReleaseAll;
    finally
      fEndCondLock.Release;
    end;
  end;
end;

procedure TThreadManager.WaitForAllThreads;
begin
  fEndCondLock.Acquire;
  try
    while fAliveThreads <> 0 do
      fEndCondition.WaitFor(fEndCondLock);
  finally
    fEndCondLock.Release;
  end;
end;

{$ENDREGION}


{$REGION 'TState'}

constructor TState.Create(const maxIters: TIterationCount; const ranges: TArray<Int64>;
  threadId, threadCount: Integer; const timer: TThreadTimer; const manager: TThreadManager);
begin
  fTotalIterations := 0;
  fBatchLeftover := 0;
  fMaxIterations := maxIters;
  fStarted := False;
  fFinished := False;
  fErrorOccurred := False;
  fRange := ranges;
  fComplexityN := 0;
  fThreadIndex := threadId;
  fThreads := threadCount;
  fTimer := timer;
  fManager := manager;
  Assert(fMaxIterations <> 0, 'At least one iteration must be run');
  Assert(fThreadIndex < fThreads, 'thread_index must be less than threads');
end;

function TState.GetEnumerator: TStateIterator;
{$IFDEF CPUX86}
var
  errorMask: Integer;
{$ENDIF}
begin
  {$IFDEF CPUX86}
  Result.fCached := fMaxIterations;
  errorMask := Integer(fErrorOccurred) - 1;
  with Int64Rec(Result.fCached) do
  begin
    Lo := Lo and errorMask;
    Hi := Hi and errorMask;
  end;
  {$ELSE}
  Result.fCached := fMaxIterations and (Int64(fErrorOccurred) - 1);
  {$ENDIF}
  Result.fParent := @Self;
  StartKeepRunning;
end;

function TState.GetBytesProcessed: UInt64;
var
  counter: PCounter;
begin
  counter := fCounters.Find('bytes_per_second');
  if counter <> nil then
    Result := Trunc(counter.value)
  else
    Result := 0;
end;

function TState.GetCounter(const name: string): TCounter;
begin
  Result := fCounters.Get(name)^;
end;

function TState.GetItemsProcessed: UInt64;
var
  counter: PCounter;
begin
  counter := fCounters.Find('items_per_second');
  if counter <> nil then
    Result := Trunc(counter.value)
  else
    Result := 0;
end;

function TState.GetIterations: TIterationCount;
begin
  if fStarted then
    Result := (fMaxIterations - fTotalIterations) + fBatchLeftover
  else
    Result := 0;
end;

function TState.GetRange(index: Integer): Int64;
begin
  Assert(index <= Length(fRange));
  Result := fRange[index];
end;

procedure TState.PauseTiming;
begin
  Assert(fStarted and not fFinished and not fErrorOccurred);
  fTimer.StopTimer;
end;

procedure TState.ResumeTiming;
begin
  Assert(fStarted and not fFinished and not fErrorOccurred);
  fTimer.StartTimer;
end;

procedure TState.SkipWithError(const msg: string);
begin
  Assert(msg <> '');
  fErrorOccurred := True;
  fManager.Lock;
  try
    if not fManager.results.hasError then
    begin
      fManager.results.errorMessage := msg;
      fManager.results.hasError := True;
    end;
  finally
    fManager.Unlock;
  end;
  fTotalIterations := 0;
  if fTimer.Running then
    fTimer.StopTimer;
end;

procedure TState.SetBytesProcessed(const bytes: UInt64);
begin
  fCounters.Get('bytes_per_second').Init(bytes, [kIsRate], kIs1024);
end;

procedure TState.SetItemsProcessed(const items: UInt64);
begin
  fCounters.Get('items_per_second').Init(items, [kIsRate]);
end;

procedure TState.SetIterationTime(const seconds: Double);
begin
  fTimer.SetIterationTime(seconds);
end;

procedure TState.SetComplexityN(const complexityN: UInt64);
begin
  fComplexityN := complexityN;
end;

procedure TState.SetCounter(const name: string; const value: TCounter);
begin
  fCounters.Get(name)^ := value;
end;

procedure TState.SetLabel(const text: string);
begin
  fManager.Lock;
  try
    fManager.results.reportLabel := text;
  finally
    fManager.Unlock;
  end;
end;

procedure TState.StartKeepRunning;
begin
  Assert(not fStarted and not fFinished);
  fStarted := True;
  if not fErrorOccurred then
  begin
    fTotalIterations := fMaxIterations;
    fManager.StartStopBarrier;
    {$IFOPT C-}
    fTimer.StartTimer;
    {$ELSE}
    ResumeTiming;
    {$ENDIF}
  end
  else
  begin
    fTotalIterations := 0;
    fManager.StartStopBarrier;
  end;
end;

procedure TState.FinishKeepRunning;
begin
  Assert(fStarted and (not fFinished or fErrorOccurred));
  if not fErrorOccurred then
    PauseTiming;
  fTotalIterations := 0;
  fFinished := True;
  fManager.StartStopBarrier;
end;

function TState.KeepRunningInternal(const n: TIterationCount;
  isBatch: Boolean): Boolean;
begin
  Assert(n > 0);
  Assert(isBatch or (n = 1));
  if fTotalIterations >= n then
  begin
    Dec(fTotalIterations, n);
    Exit(True);
  end;
  if not fStarted then
  begin
    StartKeepRunning;
    if not fErrorOccurred and (fTotalIterations >= n) then
    begin
      Dec(fTotalIterations, n);
      Exit(True);
    end;
  end;
  if isBatch and (fTotalIterations <> 0) then
  begin
    fBatchLeftover := n - fTotalIterations;
    fTotalIterations := 0;
    Exit(True);
  end;
  FinishKeepRunning;
  Result := False;
end;

function TState.KeepRunning: Boolean;
begin
  Result := KeepRunningInternal(1, False);
end;

function TState.KeepRunningBatch(const n: TIterationCount): Boolean;
begin
  Result := KeepRunningInternal(n, True);
end;

{$ENDREGION}


{$REGION 'TState.TStateIterator'}

{$IFDEF HAS_RECORD_FINALIZER}
{$IFDEF FPC}
class operator TState.TStateIterator.Initialize(var iter: TStateIterator);
begin
  iter.fParent := nil;
end;
{$ENDIF}

class operator TState.TStateIterator.Finalize(var iter: TStateIterator);
begin
  {$IFDEF FPC}
  if Assigned(iter.fParent) then
  {$ENDIF}
  iter.fParent.FinishKeepRunning;
end;
{$ENDIF}

function TState.TStateIterator.MoveNext: Boolean;
begin
{$IFDEF HAS_RECORD_FINALIZER}
  {$IFDEF CPUX86}
  Result := (Int64Rec(fCached).Lo or Int64Rec(fCached).Hi) > 0;
  {$ELSE}
  Result := fCached > 0;
  {$ENDIF}
  if Result then
    Dec(fCached);
{$ELSE}
  {$IFDEF CPUX86}
  if (Int64Rec(fCached).Lo or Int64Rec(fCached).Hi) > 0 then
  {$ELSE}
  if fCached > 0 then
  {$ENDIF}
  begin
    Dec(fCached);
    Exit(True);
  end;
  fParent.FinishKeepRunning;
  Result := False;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TBenchmark'}

constructor TBenchmark.Create(const name: string);
begin
  Assert(name <> '');
  fName := name;
  fRangeMultiplier := kRangeMultiplier;
  ComputeStatistics('mean', StatisticsMean);
  ComputeStatistics('median', StatisticsMedian);
  ComputeStatistics('stddev', StatisticsStdDev);
end;

function TBenchmark.GetArgsCount: Integer;
begin
  if fArgs = nil then Exit(-1);
  Result := Length(fArgs[0]);
end;

function TBenchmark.Arg(const x: Int64): TBenchmark;
var
  arg: TArray<Int64>;
begin
  Assert((ArgsCount = -1) or (ArgsCount = 1));
  arg := [x];
  fArgs := fArgs + [arg];
  Result := Self;
end;

function TBenchmark.TimeUnit(const timeUnit: TTimeUnit): TBenchmark;
begin
  fTimeUnit := timeUnit;
  Result := Self;
end;

function TBenchmark.Range(const start, limit: Int64): TBenchmark;
var
  argList: TArray<Int64>;
  arg: TArray<Int64>;
  i: Int64;
begin
  Assert((ArgsCount = -1) or (ArgsCount = 1));
  AddRange(arglist, start, limit, fRangeMultiplier);
  for i in argList do
  begin
    arg := [i];
    fArgs := fArgs + [arg];
  end;
  Result := Self;
end;

function TBenchmark.DenseRange(const start, limit: Int64;
  step: Integer): TBenchmark;
var
  arg: Int64;
  args: TArray<Int64>;
begin
  Assert((ArgsCount = -1) or (ArgsCount = 1));
  Assert(start <= limit);
  arg := start;
  repeat
    args := [arg];
    fArgs := fArgs + [args];
    Inc(arg, step);
  until arg > limit;
  Result := Self;
end;

function TBenchmark.Args(const args: array of Int64): TBenchmark;
var
  argList: TArray<Int64>;
begin
  Assert((ArgsCount = -1) or (ArgsCount = Length(args)));
  SetLength(argList, Length(args));
  Move(args[0], argList[0], Length(args) * SizeOf(Int64));
  fArgs := fArgs + [argList];
  Result := Self;
end;

function TBenchmark.Ranges(const ranges: array of TRange): TBenchmark;
var
  argLists: TArray<TArray<Int64>>;
  i: Integer;
begin
  Assert((ArgsCount = -1) or (ArgsCount = Length(ranges)));
  SetLength(argLists, Length(ranges));
  for i := 0 to High(ranges) do
    AddRange(argLists[i], ranges[i].start, ranges[i].limit, fRangeMultiplier);

  ArgsProduct(argLists);

  Result := Self;
end;

function TBenchmark.ArgsProduct(
  const argLists: TArray<TArray<Int64>>): TBenchmark;
var
  indices: TArray<Integer>;
  total, i, arg: Integer;
  args: TArray<Int64>;
begin
  Assert((ArgsCount = -1) or (ArgsCount = Length(argLists)));

  SetLength(indices, Length(argLists));
  total := 1;
  for i := 0 to High(argLists) do
    total := total * Length(argLists[i]);
  for i := 0 to total - 1 do
  begin
    SetLength(args, Length(argLists));
    for arg := 0 to High(argLists) do
      args[arg] := arglists[arg][indices[arg]];
    fArgs := fArgs + [args];

    for arg := 0 to High(argLists) do
    begin
      indices[arg] := (indices[arg] + 1) mod Length(argLists[arg]);
      if indices[arg] <> 0 then Break;
    end;
  end;

  Result := Self;
end;

function TBenchmark.ArgName(const name: string): TBenchmark;
begin
  Assert((ArgsCount = -1) or (ArgsCount = 1));
  fArgNames := [name];
  Result := Self;
end;

function TBenchmark.ArgNames(const names: array of string): TBenchmark;
var
  i: Integer;
begin
  Assert((ArgsCount = -1) or (ArgsCount = 1));
  SetLength(fArgNames, Length(names));
  for i := 0 to High(names) do
    fArgNames[i] := names[i];
  Result := Self;
end;

function TBenchmark.Apply(const customArguments: TCustomizeFunc): TBenchmark;
begin
  customArguments(Self);
  Result := Self;
end;

function TBenchmark.RangeMultiplier(const multiplier: Integer): TBenchmark;
begin
  Assert(multiplier > 1);
  fRangeMultiplier := multiplier;
  Result := Self;
end;

function TBenchmark.MinTime(const t: Double): TBenchmark;
begin
  Assert(t > 0);
  Assert(fIterations = 0);
  fMinTime := t;
  Result := Self;
end;

function TBenchmark.Iterations(const n: TIterationCount): TBenchmark;
begin
  Assert(n > 0);
  Assert(IsZero(fMinTime));
  fIterations := n;
  Result := Self;
end;

function TBenchmark.Repetitions(const n: Integer): TBenchmark;
begin
  Assert(n > 0);
  fRepetitions := n;
  Result := Self;
end;

function TBenchmark.ReportAggregatesOnly(const value: Boolean): TBenchmark;
begin
  if value then
    fAggregationReportMode := armReportAggregatesOnly
  else
    fAggregationReportMode := [armDefault];
  Result := Self;
end;

function TBenchmark.DisplayAggregatesOnly(const value: Boolean): TBenchmark;
begin
  // If we were called, the report mode is no longer 'unspecified', in any case.
  Include(fAggregationReportMode, armDefault);

  if value then
    Include(fAggregationReportMode, armDisplayReportAggregatesOnly)
  else
    Exclude(fAggregationReportMode, armDisplayReportAggregatesOnly);

  Result := Self;
end;

function TBenchmark.MeasureProcessCPUTime: TBenchmark;
begin
  fMeasureProcessCpuTime := True;
  Result := Self;
end;

function TBenchmark.UseRealTime: TBenchmark;
begin
  Assert(not fUseManualTime, 'Cannot set UseRealTime and UseManualTime simultaneously.');
  fUseRealTime := True;
  Result := Self;
end;

function TBenchmark.UseManualTime: TBenchmark;
begin
  Assert(not fUseRealTime, 'Cannot set UseRealTime and UseManualTime simultaneously.');
  fUseManualTime := True;
  Result := Self;
end;

function TBenchmark.Complexity(const complexity: TBigO): TBenchmark;
begin
  fComplexity := complexity;
  Result := Self;
end;

function TBenchmark.Complexity(const complexity: TBigOFunc): TBenchmark;
begin
  fComplexityLambda := complexity;
  fComplexity := oLambda;
  Result := Self;
end;

function TBenchmark.ComputeStatistics(const name: string;
  const statistics: TStatisticsFunc): TBenchmark;
begin
  fStatistics := fStatistics + [TStatistics.Create(name, statistics)];
  Result := Self;
end;

function TBenchmark.Threads(const t: Integer): TBenchmark;
begin
  Assert(t > 0);
  fThreadCounts := fThreadCounts + [t];
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TCPUInfo'}

class constructor TCPUInfo.Create;
begin
  numCpus := GetNumCPUs;
  cyclesPerSecond := GetCPUCyclesPerSecond;
  caches := GetCacheSizes;
  scaling := Unknown;
  loadAvg := nil;
end;

{$ENDREGION}


{$REGION 'TFunctionBenchmark'}

constructor TFunctionBenchmark.Create(const name: string;
  const func: TFunction);
begin
  inherited Create(name);
  fFunc := func;
end;

procedure TFunctionBenchmark.Run(const state: TState);
begin
  fFunc(state);
end;

{$ENDREGION}


{$REGION 'TBenchmarkName'}

function TBenchmarkName.Str: string;

  function join(const delimiter: Char; const s: array of string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to High(s) do
    begin
      if (Result <> '') and (s[i] <> '') then
        Result := Result + delimiter;
      Result := Result + s[i];
    end;
  end;

begin
  Result := join('/', [functionName, args, minTime, iterations, pepetitions, timeType, threads]);
end;

{$ENDREGION}


{$REGION 'TBenchmarkInstance'}

function TBenchmarkInstance.Run(const iters: TIterationCount; threadId: Integer;
  const timer: TThreadTimer; const manager: TThreadManager): TState;
var
  st: TState;
begin
  st := TState.Create(iters, arg, threadId, threads, timer, manager);
  benchmark.Run(st);
  Result := st;
end;

{$ENDREGION}


{$REGION 'TBenchmarkFamilies'}

class constructor TBenchmarkFamilies.Create;
begin
  fFamilies := TList.Create;
  fLock := TCriticalSection.Create;
end;

class destructor TBenchmarkFamilies.Destroy;
var
  i: Integer;
begin
  fLock.Free;
  for i := 0 to fFamilies.Count - 1 do
    TObject(fFamilies[i]).Free;
  fFamilies.Free;
end;

class function TBenchmarkFamilies.AddBenchmark(const family: TBenchmark): Integer;
begin
  fLock.Acquire;
  try
    Result := fFamilies.Add(family);
  finally
    fLock.Release;
  end;
end;

class procedure TBenchmarkFamilies.ClearBenchmarks;
begin
  fLock.Acquire;
  try
    fFamilies.Clear;
  finally
    fLock.Release;
  end;
end;

class function TBenchmarkFamilies.FindBenchmarks(spec: string;
  var benchmarks: TArray<TBenchmarkInstance>): Boolean;
const
  oneThread = 1;
var
//  errorMsg: string;
  re: TRegEx;
  isNegativeFilter: Boolean;
  p: Pointer;
  family: TBenchmark;
  threadCounts: TArray<Integer>;
  familySize: Integer;
  args: TArray<Int64>;
  numThreads: Integer;
  instance: TBenchmarkInstance;
  arg: Int64;
  i: Integer;
  argName, fullName: string;
begin
  isNegativeFilter := False;

  if spec[1] = '-' then
  begin
    System.Delete(spec, 1, 1);
    isNegativeFilter := True;
  end;
  re := TRegex.Create(spec);

  for p in fFamilies do
  begin
    family := p;
    if family.ArgsCount = -1 then
      family.fArgs := [[]];

    if family.fThreadCounts = nil then
      threadCounts := [oneThread]
    else
      threadCounts := family.fThreadCounts;
    familySize := Length(family.fArgs) * Length(threadCounts);
    // The benchmark will be run at least 'family_size' different inputs.
    if familySize > kMaxFamilySize then
      WriteLine('The number of inputs is very large. %s ' +
                'will be repeated at least %d times.', [family.fName, familySize]);

    for args in family.fArgs do
      for numThreads in threadCounts do
      begin
        instance := Default(TBenchmarkInstance);
        instance.name.functionName := family.fName;
        instance.benchmark := family;  //.get
        instance.aggregationReportMode := family.fAggregationReportMode;
        instance.arg := args;
        instance.timeUnit := family.fTimeUnit;
        instance.rangeMultiplier := family.fRangeMultiplier;
        instance.minTime := family.fMinTime;
        instance.iterations := family.fIterations;
        instance.repetitions := family.fRepetitions;
        instance.measureProcessCpuTime := family.fMeasureProcessCpuTime;
        instance.useRealTime := family.fUseRealTime;
        instance.useManualTime := family.fUseManualTime;
        instance.complexity := family.fComplexity;
        instance.complexityLambda := family.fComplexityLambda;
        instance.statistics := family.fStatistics;
        instance.threads := numThreads;

        // Add arguments to instance name
        i := 0;
        for arg in args do
        begin
          if instance.name.args <> '' then
            instance.name.args := instance.name.args + '/';

          if i < Length(family.fArgNames) then
          begin
            argName := family.fArgNames[i];
            if argName <> '' then
              instance.name.args := instance.name.args + Format('%s:', [argName]);
          end;

          instance.name.args := instance.name.args + arg.ToString;
          Inc(i);
        end;

        if not IsZero(family.fMinTime) then
          instance.name.minTime := Format('minTime:%0.3f', [family.fMinTime]);
        if family.fIterations <> 0 then
          instance.name.iterations := Format('iterations:%u', [family.fIterations]);
        if family.fRepetitions <> 0 then
          instance.name.pepetitions := Format('repeats:%d', [family.fRepetitions]);

        if family.fMeasureProcessCpuTime then
          instance.name.timeType := 'processTime';

        if family.fUseManualTime then
        begin
          if instance.name.timeType <> '' then
            instance.name.timeType := instance.name.timeType + '/';
          instance.name.timeType := instance.name.timeType + 'manualTime';
        end else if family.fUseRealTime then
        begin
          if instance.name.timeType <> '' then
            instance.name.timeType := instance.name.timeType + '/';
          instance.name.timeType := instance.name.timeType + 'realTime';
        end;

        // Add the number of threads used to the name
        if family.fThreadCounts <> nil then
          instance.name.threads := Format('threads:%d', [instance.threads]);

        fullName := instance.name.Str;
        if (re.IsMatch(fullName) and not isNegativeFilter) or
           (not re.IsMatch(fullName) and isNegativeFilter) then
        begin
          instance.lastBenchmarkInstance := args = family.fArgs[High(family.fArgs)];
          benchmarks := benchmarks + [instance];
        end;
      end;
    end;

  Result := True;
end;

{$ENDREGION}


{$REGION 'TBenchmarkReporter'}

procedure TBenchmarkReporter.Finalize;
begin
end;

procedure TBenchmarkReporter.PrintBasicContext(var output: System.Text;
  const context: TContext);
type
  info = TCPUInfo;
var
  i: Integer;
begin
  WriteLine(LocalDataTimeString);

  if context.executableName <> '' then
    WriteLine('Running %s', [context.executableName]);

  WriteLine('Run on (%d X %f MHz CPU %s)', [info.numCpus,
                                            info.cyclesPerSecond / 1000000,
                                            StringOfChar('s', Integer(info.numCpus>1))]);

  if info.caches <> nil then
  begin
    WriteLine('CPU Caches:');
    for i := 0 to High(info.caches) do
    begin
      Write('  L%d %s %d K', [info.caches[i].level, info.caches[i].typ, info.caches[i].size div 1024]);
      if info.caches[i].numSharing <> 0 then
        Write(' (x%d)', [info.numCpus div info.caches[i].numSharing]);
      WriteLine;
    end;
  end;

  if info.loadAvg <> nil then
  begin
    Write('Load Averge: ');
    for i := 0 to High(info.loadAvg) do
    begin
      Write('%.2f', [info.loadAvg[i]]);
      if i < High(info.loadAvg) then Write(', ');
    end;
    WriteLine;
  end;

  if TCPUInfo.TScaling.Enabled = info.scaling then
    WriteLine('***WARNING*** CPU scaling is enabled, the benchmark real time ' +
              'measurements may be noisy and will incur extra overhead.');

{$IFDEF DEBUG}
  WriteLine('***WARNING*** Library was built as DEBUG. Timings may be affected.');
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TBenchmarkReporter.TRun'}

class function TBenchmarkReporter.TRun.Create: TRun;
begin
  with Result do
  begin
    runType := rtIteration;
    errorOccurred := False;
    iterations := 1;
    threads := 1;
    timeUnit := kNanosecond;
    realAccumulatedTime := 0;
    cpuAccumulatedTime := 0;
    maxHeapBytesUsed := 0;
    complexity := oNone;
    complexityLambda := nil;
    complexityN := 0;
    reportBigO := False;
    reportRms := False;
    counters := nil;
    hasMemoryResult := False;
    allocsPerIter := 0;
    maxBytesUsed := 0;
  end;
end;

function TBenchmarkReporter.TRun.GetBenchmarkName: string;
begin
  Result := runName.Str;
  if runType = rtAggregate then
    Result := Result + '_' + aggregateName;
end;

function TBenchmarkReporter.TRun.GetAdjustedRealTime: Double;
begin
  Result := realAccumulatedTime * GetTimeUnitMultiplier(timeUnit);
  if iterations <> 0 then
    Result := Result / iterations;
end;

function TBenchmarkReporter.TRun.GetAdjustedCPUTime: Double;
begin
  Result := cpuAccumulatedTime * GetTimeUnitMultiplier(timeUnit);
  if iterations <> 0 then
    Result := Result / iterations;
end;

{$ENDREGION}


{$REGION 'TConsoleReporter'}

constructor TConsoleReporter.Create(const opts: TOutputOptions);
begin
  fOutputOptions := opts;
end;

function TConsoleReporter.ReportContext(const context: TBenchmarkReporter.TContext): Boolean;
begin
  fNameFieldWidth := context.nameFieldWidth;
  fPrintedHeader := False;
  fPrevCounters := nil;

  PrintBasicContext(Output, context);

  Result := True;
end;

procedure TConsoleReporter.PrintHeader(const run: TBenchmarkReporter.TRun);
var
  str, line: string;
  i: Integer;
begin
  str := Format('%-*s %13s %15s %12s',[fNameFieldWidth,
                                      'Benchmark', 'Time', 'CPU', 'Iterations']);

  if run.counters <> nil then
  begin
    if ooTabular in fOutputOptions then
      for i := 0 to High(run.counters) do
        str := str + Format(' %10s', [run.counters[i].name])
    else
      str := str + ' UserCounters...';
  end;

  line := StringOfChar('-', Length(str));
  WriteLine(line);
  WriteLine(str);
  WriteLine(line);
end;

procedure TConsoleReporter.ReportRuns(
  const reports: TArray<TBenchmarkReporter.TRun>);
var
  i: Integer;
  printHeader: Boolean;
begin
  for i := 0 to high(reports) do
  begin
    // print the header:
    // --- if none was printed yet
    printHeader := not fPrintedHeader;
    // --- or if the format is tabular and this run
    //     has different fields from the prev header
    printHeader := printHeader or (ooTabular in fOutputOptions) and
                   not SameNames(reports[i].counters, fPrevCounters);
    if printHeader then
    begin
      fPrintedHeader := True;
      fPrevCounters := reports[i].counters;
      Self.PrintHeader(reports[i]);
    end;
    // As an alternative to printing the headers like this, we could sort
    // the benchmarks by header and then print. But this would require
    // waiting for the full results before printing, or printing twice.
    PrintRunData(reports[i]);
  end;
end;

procedure TConsoleReporter.PrintRunData(const result: TBenchmarkReporter.TRun);

  function GetColor(const color: TColor): TColor;
  begin
    if ooColor in fOutputOptions then
      Result := color
    else
      Result := clWhite;
  end;

const
  oneKValues: array[TCounter.TOneK] of Double = (1000, 1024);
var
  realTime, cpuTime: Double;
  realTimeStr, cpuTimeStr, bigO, timeLabel: string;
  nameColor: TColor;
  i: Integer;
  nameLen: Integer;
  s, u: string;
begin
  if result.reportBigO or result.reportRms then
    nameColor := clBlue
  else
    nameColor := clGreen;
  Write('%-*s ', [fNameFieldWidth, result.BenchmarkName], GetColor(nameColor));

  if result.errorOccurred then
  begin
    WriteLine('ERROR OCCURRED: ''%s''', [result.errorMessage], GetColor(clRed));
    Exit;
  end;

  realTime := result.GetAdjustedRealTime;
  cpuTime := result.GetAdjustedCPUTime;
  realTimeStr := FormatTime(realTime);
  cpuTimeStr := FormatTime(cpuTime);

  if result.reportBigO then
  begin
    bigO := GetBigOString(result.complexity);
    Write('%10.2f %-4s %10.2f %-4s ', [realTime, bigO, cpuTime, bigO], GetColor(clYellow));
  end else if result.reportRms then
    Write('%10.0f %-4s %10.0f %-4s ', [realTime * 100, '%', cpuTime * 100, '%'], GetColor(clYellow))
  else
  begin
    timeLabel := GetTimeUnitString(result.timeUnit);
    Write('%s %-4s %s %-4s ', [realTimeStr, timeLabel, cpuTimeStr, timeLabel], GetColor(clYellow));
  end;

  if not result.reportBigO and not result.reportRms then
    Write('%10d', [result.iterations], GetColor(clCyan));

  for i := 0 to High(result.counters) do
  begin
    nameLen := Max(10, Length(result.counters[i].name));
    s := HumanReadableNumber(result.counters[i].counter.value,
                             oneKValues[result.counters[i].counter.oneK]);
    if kIsRate in result.counters[i].counter.flags then
      if kInvert in result.counters[i].counter.flags then
        u := 's'
      else
        u := '/s';
    if ooTabular in fOutputOptions then
      Write(' %*s%s', [nameLen - Length(u), s, u])
    else
      Write(' %s=%s%s', [result.counters[i].name, s, u]);
  end;

  if result.reportLabel <> '' then
    Write(result.reportLabel);

  WriteLine;
end;

{$ENDREGION}


{$REGION 'TCSVReporter'}

function TCSVReporter.ReportContext(
  const context: TBenchmarkReporter.TContext): Boolean;
begin
  Result := True;
end;

const
  elements: array[0..9] of string = (
    'name',           'iterations',       'real_time',        'cpu_time',
    'time_unit',      'bytes_per_second', 'items_per_second', 'label',
    'error_occurred', 'error_message'
  );

function CsvEscape(const s: string): string;
begin
  Result := AnsiQuotedStr(s, '"');
end;

type
  TStreamHelper = class helper for TStream
    procedure WriteBOM;
    function WriteData(const Buffer: string): NativeInt; overload;
  end;

procedure TStreamHelper.WriteBOM;
const
  BOM: Word = $FEFF;
begin
  Write(BOM, 2);
end;

function TStreamHelper.WriteData(const Buffer: string): NativeInt;
begin
  Result := Write(Pointer(Buffer)^, Length(Buffer) shl 1);
end;

procedure TCSVReporter.ReportRuns(
  const reports: TArray<TBenchmarkReporter.TRun>);
var
  i, k: Integer;
begin
  if not fPrintedHeader then
  begin
    // save the names of all the user counters
    for i := 0 to High(reports) do
      for k := 0 to High(reports[i].counters) do
      begin
        if (reports[i].counters[k].name = 'bytes_per_second') or
           (reports[i].counters[k].name = 'items_per_second') then
          Continue;
        fUserCounterNames := fUserCounterNames + [reports[i].counters[k].name];
      end;

    fOutputStream.WriteBOM;

    // print the header
    for i := 0 to High(elements) do
    begin
      fOutputStream.WriteData(elements[i]);
      if i < High(elements) then
        fOutputStream.WriteData(csv_separator);
    end;
    for i := 0 to High(fUserCounterNames) do
      fOutputStream.WriteData(csv_separator + '"' + fUserCounterNames[i] + '"');
    fOutputStream.WriteData(sLineBreak);

    fPrintedHeader := True;
  end
  else
  begin
    // check that all the current counters are saved in the name set
//    for i := 0 to High(reports) do
//    begin
//      for k := 0 to High(reports[i].counters) do
//      begin
//        if (reports[i].counters[k].name = 'bytes_per_second') or
//           (reports[i].counters[k].name = 'items_per_second') then
//          Continue;
//      end;
//    end;

//    for (const auto& run : reports) {
//      for (const auto& cnt : run.counters) {
//        if (cnt.first == "bytes_per_second" || cnt.first == "items_per_second")
//          continue;
//        CHECK(user_counter_names_.find(cnt.first) != user_counter_names_.end())
//            << "All counters must be present in each run. "
//            << "Counter named \"" << cnt.first
//            << "\" was not in a run after being added to the header";
//      }
//    }

  end;

  for i := 0 to High(reports) do
    PrintRunData(reports[i]);
end;

procedure TCSVReporter.PrintRunData(const run: TBenchmarkReporter.TRun);
const
  fmt: TFormatSettings = (DecimalSeparator: '.');
var
  counter: PCounter;
  i: Integer;
begin
  fOutputStream.WriteData(CsvEscape(run.BenchmarkName) + csv_separator);
  if run.errorOccurred then
  begin
    fOutputStream.WriteData((Length(elements) - 3).ToString + csv_separator);
    fOutputStream.WriteData('true,');
    fOutputStream.WriteData(CsvEscape(run.errorMessage));
    fOutputStream.WriteData(sLineBreak);
    Exit;
  end;

  // Do not print iteration on bigO and RMS report
  if not run.reportBigO and not run.reportRms then
    fOutputStream.WriteData(IntToStr(run.iterations));
  fOutputStream.WriteData(csv_separator);

  fOutputStream.WriteData(run.GetAdjustedRealTime.ToString(fmt) + csv_separator);
  fOutputStream.WriteData(run.GetAdjustedCPUTime.ToString(fmt) + csv_separator);

  // Do not print timeLabel on bigO and RMS report
  if run.reportBigO then
    fOutputStream.WriteData(GetBigOString(run.complexity))
  else if not run.reportRms then
    fOutputStream.WriteData(GetTimeUnitString(run.timeUnit));
  fOutputStream.WriteData(csv_separator);

  counter := run.counters.Find('bytes_per_second');
  if counter <> nil then
    fOutputStream.WriteData(counter.value.ToString(fmt));
  fOutputStream.WriteData(csv_separator);
  counter := run.counters.Find('items_per_second');
  if counter <> nil then
    fOutputStream.WriteData(counter.value.ToString(fmt));
  fOutputStream.WriteData(csv_separator);
  if run.reportLabel <> '' then
    fOutputStream.WriteData(run.reportLabel);
  fOutputStream.WriteData(csv_separator + csv_separator);  // for error_occurred and error_message

  // Print user counters
  for i := 0 to High(fUserCounterNames) do
  begin
    counter := run.counters.Find(fUserCounterNames[i]);
    fOutputStream.WriteData(csv_separator);
    if counter <> nil then
      fOutputStream.WriteData(counter.value.ToString(fmt));
  end;
  fOutputStream.WriteData(sLineBreak);
end;

{$ENDREGION}


{$REGION 'TJSONReporter'}

function StrEscape(const s: string): string;
var
  tmp: string;
  c: Char;
begin
  for c in s do
    case c of
       #8: tmp := tmp + '\b';
      #12: tmp := tmp + '\f';
      #13: tmp := tmp + '\n';
      #10: tmp := tmp + '\r';
       #9: tmp := tmp + '\t';
      '\': tmp := tmp + '\\';
      '"': tmp := tmp + '\"';
    else
           tmp := tmp + c;
    end;
  Result := tmp;
end;

function FormatKV(const key: string; const value: string): string; overload;
begin
  Result := Format('"%s": "%s"', [StrEscape(key), StrEscape(value)]);
end;

function FormatKV(const key: string; const value: Boolean): string; overload;
begin
  Result := Format('"%s": %d', [StrEscape(key), IfThen(value, 'true', 'false')]);
end;

function FormatKV(const key: string; const value: Int64): string; overload;
begin
  Result := Format('"%s": %d', [StrEscape(key), value]);
end;

function FormatKV(const key: string; const value: Double): string; overload;
const
  fmt: TFormatSettings = (DecimalSeparator: '.');
begin
  Result := Format('"%s": %s', [StrEscape(key), FloatToStr(value, fmt)]);
end;

constructor TJSONReporter.Create;
begin
  fFirstReport := True;
end;

function TJSONReporter.ReportContext(
  const context: TBenchmarkReporter.TContext): Boolean;
const
  innerIndent = '  ';
  cacheIndent = '        ';
  fmt: TFormatSettings = (DecimalSeparator: '.');
type
  info = TCPUInfo;
var
  walltimeValue: string;
  indent: string;
  i: Integer;
  ci: TCPUInfo.TCacheInfo;
begin
  fOutputStream.WriteBOM;
  fOutputStream.WriteData('{' + sLineBreak);

  // Open context block and print context information.
  fOutputStream.WriteData(innerIndent + '"context": {' + sLineBreak);
  indent := '    ';

  walltimeValue := LocalDataTimeString;
  fOutputStream.WriteData(indent + FormatKV('date', walltimeValue) + ',' + sLineBreak);

  if context.executableName <> '' then
    fOutputStream.WriteData(indent + FormatKV('executable', context.executableName) + ',' + sLineBreak);

  fOutputStream.WriteData(indent + FormatKV('num_cpus', info.numCpus) + ',' + sLineBreak);
  fOutputStream.WriteData(indent + FormatKV('mhz_per_cpu', Round(info.cyclesPerSecond / 1000000)) + ',' + sLineBreak);
  if info.scaling <> TCPUInfo.TScaling.Unknown then
    fOutputStream.WriteData(indent + FormatKV('cpu_scaling_enabled',
    IfThen(info.scaling <> TCPUInfo.TScaling.Enabled, 'true', 'false')) + ',' + sLineBreak);

  fOutputStream.WriteData(indent + '"caches": [' + sLineBreak);
  indent := '      ';
  for i := 0 to High(info.caches) do
  begin
    ci := info.caches[i];
    fOutputStream.WriteData(indent + '{' + sLineBreak);
    fOutputStream.WriteData(cacheIndent + FormatKV('type', ci.typ) + ',' + sLineBreak);
    fOutputStream.WriteData(cacheIndent + FormatKV('level', ci.level) + ',' + sLineBreak);
    fOutputStream.WriteData(cacheIndent + FormatKV('size', ci.size) + ',' + sLineBreak);
    fOutputStream.WriteData(cacheIndent + FormatKV('num_sharing', ci.numSharing) + sLineBreak);
    fOutputStream.WriteData(indent + '}');
    if i < High(info.caches) then
      fOutputStream.WriteData(',');
    fOutputStream.WriteData(sLineBreak);
  end;

  indent := '    ';
  fOutputStream.WriteData(indent + '],' + sLineBreak);
  fOutputStream.WriteData(indent + '"load_avg": [');
  for i := 0 to High(info.loadAvg) do
  begin
    fOutputStream.WriteData(FloatToStr(info.loadAvg[i], fmt));
    if i < High(info.loadAvg) then
      fOutputStream.WriteData(',');
  end;
  fOutputStream.WriteData('],' + sLineBreak);

  fOutputStream.WriteData(indent + FormatKV('library_build_type', {$IFDEF DEBUG}'debug'{$ELSE}'release'{$ENDIF}) + sLineBreak);
  fOutputStream.WriteData(innerIndent + '},' + sLineBreak);
  fOutputStream.WriteData(innerIndent + '"benchmarks": [' + sLineBreak);
  Result := True;
end;

procedure TJSONReporter.ReportRuns(
  const reports: TArray<TBenchmarkReporter.TRun>);
const
  indent = '    ';
var
  i: Integer;
begin
  if reports = nil then
    Exit;

  if not fFirstReport then
    fOutputStream.WriteData(',' + sLineBreak);
  fFirstReport := False;

  for i := 0 to High(reports) do
  begin
    fOutputStream.WriteData(indent + '{' + sLineBreak);
    PrintRunData(reports[i]);
    fOutputStream.WriteData(indent + '}');
    if i < High(reports) then
      fOutputStream.WriteData(',' + sLineBreak);
  end;
end;

procedure TJSONReporter.Finalize;
begin
  fOutputStream.WriteData(sLineBreak + '  ]' + sLineBreak + '}' + sLineBreak);
end;

procedure TJSONReporter.PrintRunData(const run: TBenchmarkReporter.TRun);
const
  runTypeName: array[TRun.TRunType] of string = ('iteration', 'aggregate');
var
  indent: string;
  i: Integer;
begin
  indent := '      ';
  fOutputStream.WriteData(indent + FormatKV('name', run.BenchmarkName) + ',' + sLineBreak);
  fOutputStream.WriteData(indent + FormatKV('run_name', run.runName.Str) + ',' + sLineBreak);
  fOutputStream.WriteData(indent + FormatKV('run_type', runTypeName[run.runType]) + ',' + sLineBreak);
  fOutputStream.WriteData(indent + FormatKV('repetitions', run.repetitions) + ',' + sLineBreak);
  if run.runType <> TRun.TRunType.rtAggregate then
    fOutputStream.WriteData(indent + FormatKV('repetition_index', run.repetitionIndex) + ',' + sLineBreak);
  fOutputStream.WriteData(indent + FormatKV('threads', run.threads) + ',' + sLineBreak);
  if run.runType = TRun.TRunType.rtAggregate then
    fOutputStream.WriteData(indent + FormatKV('aggregate_name', run.aggregateName) + ',' + sLineBreak);
  if run.errorOccurred then
  begin
    fOutputStream.WriteData(indent + FormatKV('error_occured', run.errorOccurred) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('error_message', run.errorMessage) + ',' + sLineBreak);
  end;
  if not run.reportBigO and not run.reportRms then
  begin
    fOutputStream.WriteData(indent + FormatKV('iterations', run.iterations) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('real_time', run.GetAdjustedRealTime) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('cpu_time', run.GetAdjustedCPUTime) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('time_unit', GetTimeUnitString(run.timeUnit)));
  end else if run.reportBigO then
  begin
    fOutputStream.WriteData(indent + FormatKV('cpu_coefficient', run.GetAdjustedCPUTime) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('real_coefficient', run.GetAdjustedRealTime) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('big_o', GetBigOString(run.complexity)) + ',' + sLineBreak);
    fOutputStream.WriteData(indent + FormatKV('time_unit', GetTimeUnitString(run.timeUnit)));
  end else if run.reportRms then
    fOutputStream.WriteData(indent + FormatKV('rms', run.GetAdjustedCPUTime));

  for i := 0 to High(run.counters) do
    fOutputStream.WriteData(',' + sLineBreak + indent + FormatKV(run.counters[i].name, run.counters[i].counter.value));

  if run.hasMemoryResult then
  begin
    fOutputStream.WriteData(',' + sLineBreak + indent + FormatKV('allocs_per_iter', run.allocsPerIter));
    fOutputStream.WriteData(',' + sLineBreak + indent + FormatKV('max_bytes_used', run.maxBytesUsed));
  end;

  if run.reportLabel <> '' then
    fOutputStream.WriteData(',' + sLineBreak + indent + FormatKV('label', run.reportLabel));

  fOutputStream.WriteData(sLineBreak);
end;

{$ENDREGION}


{$REGION 'TBenchmarkRunner'}

constructor TBenchmarkRunner.Create(const benchmark: TBenchmarkInstance;
  var complexityReports: TArray<TBenchmarkReporter.TRun>);
var
  repetitionNum: Integer;
  additionalRunStats: TArray<TBenchmarkReporter.TRun>;
begin
  Self.benchmark := benchmark;
  Self.complexityReports := @complexityReports;
  minTime := benchmark.minTime; if IsZero(minTime) then minTime := benchmark_min_time;
  repeats := benchmark.repetitions; if repeats = 0 then repeats := benchmark_repetitions;
  hasExplicitIterationCount := benchmark.iterations <> 0;
  SetLength(pool, benchmark.threads - 1);
  if hasExplicitIterationCount then iters := benchmark.iterations else iters := 1;

  runResults.displayReportAggregatesOnly :=
    benchmark_report_aggregates_only or
    benchmark_display_aggregates_only;
  runResults.fileReportAggregatesOnly :=
    benchmark_report_aggregates_only;
  if benchmark.aggregationReportMode <> armUnspecified then
  begin
    runResults.displayReportAggregatesOnly :=
      armDisplayReportAggregatesOnly in benchmark.aggregationReportMode;
    runResults.fileReportAggregatesOnly :=
      armFileReportAggregatesOnly in benchmark.aggregationReportMode;
  end;

  for repetitionNum := 0 to repeats - 1 do
    DoOneRepetition(repetitionNum);

  // Calculate additional statistics
  runResults.aggregatesOnly := ComputeStats(runResults.nonAggregates);

  // Maybe calculate complexity report
  if (benchmark.complexity <> oNone) and benchmark.lastBenchmarkInstance then
  begin
    additionalRunStats := ComputeBigO(Self.complexityReports^);
    runResults.aggregatesOnly := runResults.aggregatesOnly + additionalRunStats;
    self.complexityReports^ := nil;
  end;
end;

type
  TBenchmarkThread = class(TThread)
  private
    fBenchmark: ^TBenchmarkInstance;
    fIters: TIterationCount;
    fThreadId: Integer;
    fManager: TThreadManager;
  protected
    procedure Execute; override;
  public
    constructor Create(const benchmark: TBenchmarkInstance; iters: TIterationCount;
      threadId: Integer; const manager: TThreadManager);
  end;

constructor TBenchmarkThread.Create(const benchmark: TBenchmarkInstance;
  iters: TIterationCount; threadId: Integer; const manager: TThreadManager);
begin
  inherited Create(False);
  fBenchmark := @benchmark;
  fIters := iters;
  fThreadId := threadId;
  fManager := manager;
end;

procedure TBenchmarkThread.Execute;
begin
  RunInThread(fBenchmark^, fIters, fThreadId, fManager);
end;

function TBenchmarkRunner.DoNIterations: TIterationResults;
var
  ti: Integer;
  manager: TThreadManager;
  i: TIterationResults;
begin
//  VLOG(2) << "Running " << b.name.str() << " for " << iters << "\n";

  manager := TThreadManager.Create(benchmark.threads);
  try
    // Run all but one thread in separate threads
    for ti := 0 to High(pool) do
      pool[ti] := TBenchmarkThread.Create(benchmark, iters, ti + 1, manager);

    // And run one thread here directly.
    // (If we were asked to run just one thread, we don't create new threads.)
    // Yes, we need to do this here *after* we start the separate threads.
    RunInThread(benchmark, iters, 0, manager);

    // The main thread has finished. Now let's wait for the other threads.
    manager.WaitForAllThreads;
    for ti := 0 to High(pool) do
      pool[ti].Free;

    // Acquire the measurements/counters from the manager
    i.results := manager.Results;
  finally
    // And get rid of the manager.
    manager.Free;
  end;

  // Adjust real/manual time stats since they were reported per thread.
  i.results.realTimeUsed := i.results.realTimeUsed / benchmark.threads;
  i.results.manualTimeUsed := i.results.manualTimeUsed / benchmark.threads;
  // If we were measuring whole-process CPU usage, adjust the CPU time too.
  if benchmark.measureProcessCpuTime then
    i.results.cpuTimeUsed := i.results.cpuTimeUsed / benchmark.threads;

//    VLOG(2) << "Ran in " << i.results.cpu_time_used << "/"
//            << i.results.real_time_used << "\n";

  // So for how long were we running?
  i.iters := i.results.iterations div benchmark.threads;

  // Base decisions off of real time if requested by this benchmark.
  i.seconds := i.results.CpuTimeUsed;
  if benchmark.useManualTime then
    i.seconds := i.results.ManualTimeUsed
  else if benchmark.useRealTime then
    i.seconds := i.results.RealTimeUsed;

  Result := i;
end;

function TBenchmarkRunner.PredictNumItersNeeded(
  const i: TIterationResults): TIterationCount;
var
  multiplier: Double;
  isSignificant: Boolean;
  maxNextIters, nextIters: TIterationCount;
begin
  // See how much iterations should be increased by.
  // Note: Avoid division by zero with max(seconds, 1ns).
  multiplier := minTime * 1.4 / Max(i.seconds, 1e-9);
  // If our last run was at least 10% of FLAGS_benchmark_min_time then we
  // use the multiplier directly.
  // Otherwise we use at most 10 times expansion.
  // NOTE: When the last run was at least 10% of the min time the max
  // expansion should be 14x.
  isSignificant := (i.seconds / minTime) > 0.1;
  if not isSignificant then
    multiplier := Min(10.0, multiplier);
  if multiplier <= 1.0 then
    multiplier := 2.0;

  // So what seems to be the sufficiently-large iteration count? Round up.
  maxNextIters := Round(Max(multiplier * i.iters, i.iters + 1));
  // But we do have *some* sanity limits though..
  nextIters := Min(maxNextIters, kMaxIterations);

//    VLOG(3) << "Next iters: " << next_iters << ", " << multiplier << "\n";
  Result := nextIters;
end;

function TBenchmarkRunner.ShouldReportIterationResults(
  const i: TIterationResults): Boolean;
begin
  // Determine if this run should be reported;
  // Either it has run for a sufficient amount of time
  // or because an error was reported.
  Result := i.results.hasError or
            (i.iters >= kMaxIterations) or  // Too many iterations already.
            (i.seconds >= minTime) or       // The elapsed time is large enough.
            // CPU time is specified but the elapsed real time greatly exceeds
            // the minimum time.
            // Note that user provided timers are except from this sanity check.
            ((i.results.realTimeUsed >= 5 * minTime) and not benchmark.useManualTime);
end;

procedure TBenchmarkRunner.DoOneRepetition(repetitionIndex: Int64);
var
  isTheFirstRepetition: Boolean;
  i: TIterationResults;
  resultsAreSignificant: Boolean;
  report: TBenchmarkReporter.TRun;
  memoryIterations: TIterationCount;
begin
  isTheFirstRepetition := repetitionIndex = 0;

  // We *may* be gradually increasing the length (iteration count)
  // of the benchmark until we decide the results are significant.
  // And once we do, we report those last results and exit.
  // Please do note that the if there are repetitions, the iteration count
  // is *only* calculated for the *first* repetition, and other repetitions
  // simply use that precomputed iteration count.
  while True do
  begin
    i := DoNIterations;

    // Do we consider the results to be significant?
    // If we are doing repetitions, and the first repetition was already done,
    // it has calculated the correct iteration time, so we have run that very
    // iteration count just now. No need to calculate anything. Just report.
    // Else, the normal rules apply.
    resultsAreSignificant := not isTheFirstRepetition or
                             hasExplicitIterationCount or
                             ShouldReportIterationResults(i);

    if resultsAreSignificant then Break;  // Good, let's report them!

    // Nope, bad iteration. Let's re-estimate the hopefully-sufficient
    // iteration count, and run the benchmark again...

    iters := PredictNumItersNeeded(i);
    Assert(iters > i.iters,
           'if we did more iterations than we want to do the next time, ' +
           'then we should have accepted the current iteration run.');
  end;

//    // Oh, one last thing, we need to also produce the 'memory measurements'..
//    MemoryManager::Result memory_result;
    memoryIterations := 0;
//    if (memory_manager != nullptr) {
//      // Only run a few iterations to reduce the impact of one-time
//      // allocations in benchmarks that are not properly managed.
//      memory_iterations = std::min<IterationCount>(16, iters);
//      memory_manager->Start();
//      std::unique_ptr<internal::ThreadManager> manager;
//      manager.reset(new internal::ThreadManager(1));
//      RunInThread(&b, memory_iterations, 0, manager.get());
//      manager->WaitForAllThreads();
//      manager.reset();
//
//      memory_manager->Stop(&memory_result);
//    }

  // Ok, now actualy report.
  report := CreateRunReport(benchmark, i.results, memoryIterations,{ memory_result,}
                            i.seconds, repetitionIndex);

  if not report.errorOccurred and (benchmark.complexity <> oNone) then
    complexityReports^ := complexityReports^ + [report];

  runResults.nonAggregates := runResults.nonAggregates + [report];
end;

function TBenchmarkRunner.GetResults: TRunResults;
begin
  Result := runResults;
end;

{$ENDREGION}


end.
