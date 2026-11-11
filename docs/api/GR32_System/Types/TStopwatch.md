---
layout: doc
docType: api
unit: GR32_System
entity: TStopwatch
kind: Type
summary: "High-precision timer structure for measuring elapsed time and benchmarking performance."
declaration: |
  type
    TStopwatch = record
    public const
      TicksPerMicrosecond = 10;
      TicksPerNanosecond = TicksPerMicrosecond / 1000;
      TicksPerMillisecond = 1000 * Int64(TicksPerMicrosecond);
      TicksPerSecond = 1000 * Int64(TicksPerMillisecond);
      TicksPerMinute = 60 * Int64(TicksPerSecond);
      TicksPerHour = 60 * Int64(TicksPerMinute);
      TicksPerDay = 24 * TicksPerHour;
    public
      class function Create: TStopwatch; static;
      class function GetTimeStamp: Int64; static;
      procedure Reset;
      procedure Start;
      class function StartNew: TStopwatch; static;
      procedure Stop;

      property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
      property ElapsedTicks: Int64 read GetElapsedTicks;
      class property Frequency: Int64 read FFrequency;
      class property IsHighResolution: Boolean read FIsHighResolution;
      property IsRunning: Boolean read FRunning;
    end;
---

## Description

`TStopwatch` measures elapsed time using high-resolution performance counters where supported by the underlying operating system and hardware platform. On Delphi, `TStopwatch` wraps `System.Diagnostics.TStopWatch` with helper tick conversion constants. On Free Pascal (FPC), `TStopwatch` is implemented cross-platform (Windows QueryPerformanceCounter, POSIX `clock_gettime`, macOS `mach_absolute_time`).

## Constants

| Constant | Value / Definition | Description |
| --- | --- | --- |
| `TicksPerMicrosecond` | `10` | Number of timer ticks in one microsecond (1 tick = 100 ns). |
| `TicksPerNanosecond` | `TicksPerMicrosecond / 1000` | Timer ticks per nanosecond. |
| `TicksPerMillisecond` | `1000 * TicksPerMicrosecond` | Timer ticks per millisecond. |
| `TicksPerSecond` | `1000 * TicksPerMillisecond` | Timer ticks per second. |
| `TicksPerMinute` | `60 * TicksPerSecond` | Timer ticks per minute. |
| `TicksPerHour` | `60 * TicksPerMinute` | Timer ticks per hour. |
| `TicksPerDay` | `24 * TicksPerHour` | Timer ticks per day. |

## Methods

| Method | Description |
| --- | --- |
| `Create` | Creates a new stopped `TStopwatch` instance. |
| `StartNew` | Creates and immediately starts a new `TStopwatch` timer. |
| `GetTimeStamp` | Class static function returning the current high-resolution timestamp tick count. |
| `Start` | Starts or resumes measuring elapsed time. |
| `Stop` | Stops measuring elapsed time. |
| `Reset` | Stops time measurement and resets elapsed ticks to 0. |

## Properties

| Property | Type | Description |
| --- | --- | --- |
| `ElapsedMilliseconds` | `Int64` | Total elapsed time measured by this instance in milliseconds. |
| `ElapsedTicks` | `Int64` | Total elapsed timer ticks measured by this instance. |
| `Frequency` | `Int64` | Static class property returning the frequency of the timer in ticks per second. |
| `IsHighResolution` | `Boolean` | Static class property indicating whether the timer uses a high-resolution performance counter. |
| `IsRunning` | `Boolean` | Returns `True` if the stopwatch is currently active and measuring time. |

## Example

```pascal
var
  Stopwatch: TStopwatch;
begin
  Stopwatch := TStopwatch.StartNew;
  try
    // Perform benchmark operation
    DoComplexGraphicsProcessing;
  finally
    Stopwatch.Stop;
  end;

  Writeln('Elapsed time: ', Stopwatch.ElapsedMilliseconds, ' ms');
end;
```
