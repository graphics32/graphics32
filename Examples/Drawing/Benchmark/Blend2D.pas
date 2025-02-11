unit Blend2D;
{ OPP wrappers for Blend2D C API.

  Follows the C++ API where possible. The following C++ classes are not
  converted because Delphi provides built-in alternatives for these:
  * BLArray<T>: uses TArray<T> instead
  * BLFile: uses TFileStream instead
  * BLString: uses String instead
  * BLVariant: not needed
  * BLStyle: style properties are used in another way. We may introduce a
    TBLStyle type once Delphi has support for managed records (or use Scope
    solution like TBLStrokeOptions). }

{$SCOPEDENUMS ON}
{$EXCESSPRECISION OFF}

interface

uses
  System.Math,
  System.SysUtils,
  Blend2D.Api;

{$REGION 'Error Handling'}

{ ============================================================================
   [Error Handling - Enums]
  ============================================================================ }

type
  { Blend2D result code. }
  TBLResultCode = (
    { Successful result code. }
    Success                   = BL_SUCCESS,

    { Out of memory                 [ENOMEM]. }
    OutOfMemory               = BL_ERROR_OUT_OF_MEMORY,

    { Invalid value/argument        [EINVAL]. }
    InvalidValue              = BL_ERROR_INVALID_VALUE,

    { Invalid state                 [EFAULT]. }
    InvalidState              = BL_ERROR_INVALID_STATE,

    { Invalid handle or file.       [EBADF]. }
    InvalidHandle             = BL_ERROR_INVALID_HANDLE,

    { Value too large               [EOVERFLOW]. }
    ValueTooLarge             = BL_ERROR_VALUE_TOO_LARGE,

    { Not initialized (some instance is built-in none when it shouldn't be). }
    NotInitialized            = BL_ERROR_NOT_INITIALIZED,

    { Not implemented               [ENOSYS]. }
    NotImplemented            = BL_ERROR_NOT_IMPLEMENTED,

    { Operation not permitted       [EPERM]. }
    NotPermitted              = BL_ERROR_NOT_PERMITTED,

    { IO error                      [EIO]. }
    IOError                   = BL_ERROR_IO,

    { Device or resource busy       [EBUSY]. }
    Busy                      = BL_ERROR_BUSY,

    { Operation interrupted         [EINTR]. }
    Interrupted               = BL_ERROR_INTERRUPTED,

    { Try again                     [EAGAIN]. }
    TryAgain                  = BL_ERROR_TRY_AGAIN,

    { Timed out                     [ETIMEDOUT]. }
    TimedOut                  = BL_ERROR_TIMED_OUT,

    { Broken pipe                   [EPIPE]. }
    BrokenPipe                = BL_ERROR_BROKEN_PIPE,

    { File is not seekable          [ESPIPE]. }
    InvalidSeek               = BL_ERROR_INVALID_SEEK,

    { Too many levels of symlinks   [ELOOP]. }
    SymlinkLoop               = BL_ERROR_SYMLINK_LOOP,

    { File is too large             [EFBIG]. }
    FileTooLarge              = BL_ERROR_FILE_TOO_LARGE,

    { File/directory already exists [EEXIST]. }
    AlreadyExists             = BL_ERROR_ALREADY_EXISTS,

    { Access denied                 [EACCES]. }
    AccessDenied              = BL_ERROR_ACCESS_DENIED,

    { Media changed                 [Windows::ERROR_MEDIA_CHANGED]. }
    MediaChanged              = BL_ERROR_MEDIA_CHANGED,

    { The file/FS is read-only      [EROFS]. }
    ReadOnlyFS                = BL_ERROR_READ_ONLY_FS,

    { Device doesn't exist          [ENXIO]. }
    NoDevice                  = BL_ERROR_NO_DEVICE,

    { Not found, no entry (fs)      [ENOENT]. }
    NoEntry                   = BL_ERROR_NO_ENTRY,

    { No media in drive/device      [ENOMEDIUM]. }
    NoMedia                   = BL_ERROR_NO_MEDIA,

    { No more data / end of file    [ENODATA]. }
    NoMoreData                = BL_ERROR_NO_MORE_DATA,

    { No more files                 [ENMFILE]. }
    NoMoreFiles               = BL_ERROR_NO_MORE_FILES,

    { No space left on device       [ENOSPC]. }
    NoSpaceLeft               = BL_ERROR_NO_SPACE_LEFT,

    { Directory is not empty        [ENOTEMPTY]. }
    NotEmpty                  = BL_ERROR_NOT_EMPTY,

    { Not a file                    [EISDIR]. }
    NotFile                   = BL_ERROR_NOT_FILE,

    { Not a directory               [ENOTDIR]. }
    NotDirectory              = BL_ERROR_NOT_DIRECTORY,

    { Not same device               [EXDEV]. }
    NotSameDevice             = BL_ERROR_NOT_SAME_DEVICE,

    { Not a block device            [ENOTBLK]. }
    NotBlockDevice            = BL_ERROR_NOT_BLOCK_DEVICE,

    { File/path name is invalid     [n/a]. }
    InvalidFilename           = BL_ERROR_INVALID_FILE_NAME,

    { File/path name is too long    [ENAMETOOLONG]. }
    FilenameTooLong           = BL_ERROR_FILE_NAME_TOO_LONG,

    { Too many open files           [EMFILE]. }
    TooManyOpenFiles          = BL_ERROR_TOO_MANY_OPEN_FILES,

    { Too many open files by OS     [ENFILE]. }
    TooManyOpenFilesByOS      = BL_ERROR_TOO_MANY_OPEN_FILES_BY_OS,

    { Too many symbolic links on FS [EMLINK]. }
    TooManyLinks              = BL_ERROR_TOO_MANY_LINKS,

    { Too many threads              [EAGAIN]. }
    TooManyThreads            = BL_ERROR_TOO_MANY_THREADS,

    { Thread pool is exhausted and couldn't acquire the requested thread count. }
    ThreadPoolExhausted       = BL_ERROR_THREAD_POOL_EXHAUSTED,

    { File is empty (not specific to any OS error). }
    FileEmpty                 = BL_ERROR_FILE_EMPTY,

    { File open failed              [Windows::ERROR_OPEN_FAILED]. }
    OpenFailed                = BL_ERROR_OPEN_FAILED,

    { Not a root device/directory   [Windows::ERROR_DIR_NOT_ROOT]. }
    NotRootDevice             = BL_ERROR_NOT_ROOT_DEVICE,

    { Unknown system error that failed to translate to Blend2D result code. }
    UnknownSystemError        = BL_ERROR_UNKNOWN_SYSTEM_ERROR,

    { Invalid data alignment. }
    InvalidArgument           = BL_ERROR_INVALID_ALIGNMENT,

    { Invalid data signature or header. }
    InvalidSignature          = BL_ERROR_INVALID_SIGNATURE,

    { Invalid or corrupted data. }
    InvalidData               = BL_ERROR_INVALID_DATA,

    { Invalid string (invalid data of either UTF8, UTF16, or UTF32). }
    InvalidString             = BL_ERROR_INVALID_STRING,

    { Truncated data (more data required than memory/stream provides). }
    DataTruncated             = BL_ERROR_DATA_TRUNCATED,

    { Input data too large to be processed. }
    DataTooLarge              = BL_ERROR_DATA_TOO_LARGE,

    { Decompression failed due to invalid data (RLE, Huffman, etc). }
    DecompressionFailed       = BL_ERROR_DECOMPRESSION_FAILED,

    { Invalid geometry (invalid path data or shape). }
    InvalidGeometry           = BL_ERROR_INVALID_GEOMETRY,

    { Returned when there is no matching vertex in path data. }
    NoMatchingVertex          = BL_ERROR_NO_MATCHING_VERTEX,

    { No matching cookie (BLContext). }
    NoMatchingCookie          = BL_ERROR_NO_MATCHING_COOKIE,

    { No states to restore (BLContext). }
    NoStatesToRestore         = BL_ERROR_NO_STATES_TO_RESTORE,

    { The size of the image is too large. }
    ImageTooBig               = BL_ERROR_IMAGE_TOO_LARGE,

    { Image codec for a required format doesn't exist. }
    ImageNoMatchingCodec      = BL_ERROR_IMAGE_NO_MATCHING_CODEC,

    { Unknown or invalid file format that cannot be read. }
    ImageUnknownFileFormat    = BL_ERROR_IMAGE_UNKNOWN_FILE_FORMAT,

    { Image codec doesn't support reading the file format. }
    ImageDecoderNotProvided   = BL_ERROR_IMAGE_DECODER_NOT_PROVIDED,

    { Image codec doesn't support writing the file format. }
    ImageEncoderNotProvided   = BL_ERROR_IMAGE_ENCODER_NOT_PROVIDED,

    { Multiple IHDR chunks are not allowed (PNG). }
    PngMultipleIHDR           = BL_ERROR_PNG_MULTIPLE_IHDR,

    { Invalid IDAT chunk (PNG). }
    PngInvalidIDAT            = BL_ERROR_PNG_INVALID_IDAT,

    { Invalid IEND chunk (PNG). }
    PngInvalidIEND            = BL_ERROR_PNG_INVALID_IEND,

    { Invalid PLTE chunk (PNG). }
    PngInvalidPLTE            = BL_ERROR_PNG_INVALID_PLTE,

    { Invalid tRNS chunk (PNG). }
    PngInvalidTRNS            = BL_ERROR_PNG_INVALID_TRNS,

    { Invalid filter type (PNG). }
    PngInvalidFilter          = BL_ERROR_PNG_INVALID_FILTER,

    { Unsupported feature (JPEG). }
    JpegUnsupportedFeature    = BL_ERROR_JPEG_UNSUPPORTED_FEATURE,

    { Invalid SOS marker or header (JPEG). }
    JpegInvalidSOS            = BL_ERROR_JPEG_INVALID_SOS,

    { Invalid SOF marker (JPEG). }
    JpegInvalidSOF            = BL_ERROR_JPEG_INVALID_SOF,

    { Multiple SOF markers (JPEG). }
    JpegMultipleSOF           = BL_ERROR_JPEG_MULTIPLE_SOF,

    { Unsupported SOF marker (JPEG). }
    JpegUnsupportedSOF        = BL_ERROR_JPEG_UNSUPPORTED_SOF,

    { Font doesn't have any data as it's not initialized. }
    FontNotInitialized        =  BL_ERROR_FONT_NOT_INITIALIZED,

    { Font or font-face was not matched (BLFontManager). }
    FontNoMatch               = BL_ERROR_FONT_NO_MATCH,

    { Font has no character to glyph mapping data. }
    FontNoCharacterMapping    = BL_ERROR_FONT_NO_CHARACTER_MAPPING,

    { Font has missing an important table. }
    FontMissingImportantTable = BL_ERROR_FONT_MISSING_IMPORTANT_TABLE,

    { Font feature is not available. }
    FontFeatureNotAvailable   = BL_ERROR_FONT_FEATURE_NOT_AVAILABLE,

    { Font has an invalid CFF data. }
    FontCFFInvalidData        = BL_ERROR_FONT_CFF_INVALID_DATA,

    { Font program terminated because the execution reached the limit. }
    FontProgramTerminated     = BL_ERROR_FONT_PROGRAM_TERMINATED,

    { Invalid glyph identifier. }
    InvalidGlyph              = BL_ERROR_INVALID_GLYPH);

type
  _TBLResultCodeHelper = record helper for TBLResultCode
  public
    function ToString: String;
  end;

{ ============================================================================
   [Error Handling - Handlers]
  ============================================================================ }

type
  { Type of exception that is raised for Blend2D errors.
    Exceptions are enabled by default, but can be disabled using
    BLSetErrorHandler. }
  EBlend2DError = class(Exception)
  {$REGION 'Internal Declarations'}
  private
    FResultCode: TBLResultCode;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AResultCode: TBLResultCode);

    { The Blend2D result code }
    property ResultCode: TBLResultCode read FResultCode;
  end;

type
  { Type of procedure that is called when a Blend2D error occurs.

    Parameters:
      AResultCode: the Blend2D result code
      AUserData: any user data passed to BLSetErrorHandler. }
  TBLErrorHandler = procedure(const AResultCode: TBLResultCode;
    const AUserData: Pointer);

{ Sets a Blend2D error handler.

  Parameters:
    AHandler: the error handler that is called when a Blend2D error occurs.
    AUserData: any data you want to pass to the handler.

  The default error handler raises an exception of type EBlend2DError.
  You can disable error handling completely by setting AHandler to nil. In that
  case, there is no way to know if and when an error occured.

  The following procedures can be used to set some default error handlers:
  * BLSetExceptionErrorHandler: sets the error handler to a procedure that
    raises an exception when a Blend2D error occurs. This is the default
    behavior.
  * BLSetGetLastErrorHandler: sets the error handler to a procedure that sets
    a global error variable when a Blend2D error occurs. You can then use
    BLGetLastError to retrieve this error code. }
procedure BLSetErrorHandler(const AHandler: TBLErrorHandler;
  const AUserData: Pointer);

{ Sets the error handler to a procedure that raises an exception when a Blend2D
  error occurs. This is the default behavior. }
procedure BLSetExceptionErrorHandler;

{ Sets the error handler to a procedure that sets a global error variable when a
  Blend2D error occurs. You can then use BLGetLastError to retrieve this error
  code. }
procedure BLSetGetLastErrorHandler;

{ Retrieves the last Blend2D error, or TBLResultCode.Success if there was
  none.

  After this call, the last error is reset to TBLResultCode.Success.

  This function should only be used when BLSetGetLastErrorHandler has been
  called. Otherwise, it always returns TBLResultCode.Success.

  This function is not thread-safe. If Blend2D is used from multiple threads,
  the returned value can be from any thread. }
function BLGetLastError: TBLResultCode;

{$ENDREGION 'Error Handling'}

{$REGION 'General'}

{ ============================================================================
   [General - Types]
  ============================================================================ }

type
  { Tag is a 32-bit integer consisting of 4 characters in the following format:

      Tag := (A shl 24) or (B shl 16) or (C shl 8) or D

    Tags are used extensively by OpenType fonts and other binary formats like
    PNG. In most cases TAGs should only contain ASCII letters, digits, and
    spaces.

    Blend2D uses TBLTag in public and internal APIs to distinguish between a
    regular UInt32 and tag. }
  TBLTag = BLTag;

type
  { Unique identifier that can be used for caching purposes.

    Some objects such as IBLImage and IBLFontFace have assigned an unique
    identifier that can be used to identify such objects for caching purposes.
    This identifier is never zero, so zero can be safely used as "uncached".

    Note: Unique identifier is per-process. It's implemented as an increasing
    global or thread-local counter in a way that identifiers would not
    collide. }
  TBLUniqueId = BLUniqueId;

type
  { A type used to store a pack of bits.
    BitWord should be equal in size to a machine word. }
  TBLBitWord = BLBitWord;

{ ============================================================================
   [General - Enums]
  ============================================================================ }

type
  { Boolean operator. }
  TBLBooleanOp = (
    { Result = B. }
    Copy = BL_BOOLEAN_OP_COPY,

    { Result = A and B. }
    &And = BL_BOOLEAN_OP_AND,

    { Result = A or B. }
    &Or  = BL_BOOLEAN_OP_OR,

    { Result = A xor B. }
    &Xor = BL_BOOLEAN_OP_XOR,

    { Result = A and not B. }
    Sub  = BL_BOOLEAN_OP_SUB);

type
  { Extend mode. }
  TBLExtendMode = (
    { Pad extend [default]. }
    Pad              = BL_EXTEND_MODE_PAD,

    { Repeat extend. }
    &Repeat          = BL_EXTEND_MODE_REPEAT,

    { Reflect extend. }
    Reflect          = BL_EXTEND_MODE_REFLECT,

    { Alias to Pad. }
    PadXPadY         = BL_EXTEND_MODE_PAD_X_PAD_Y,

    { Alias to Repeat. }
    RepeatXRepeatY   = BL_EXTEND_MODE_REPEAT_X_REPEAT_Y,

    { Alias to Reflect. }
    ReflectXReflectY = BL_EXTEND_MODE_REFLECT_X_REFLECT_Y,

    { Pad X and repeat Y. }
    PadXRepeatY      = BL_EXTEND_MODE_PAD_X_REPEAT_Y,

    { Pad X and reflect Y. }
    PadXReflectY     = BL_EXTEND_MODE_PAD_X_REFLECT_Y,

    { Repeat X and pad Y. }
    RepeatXPadY      = BL_EXTEND_MODE_REPEAT_X_PAD_Y,

    { Repeat X and reflect Y. }
    RepeatXReflectY  = BL_EXTEND_MODE_REPEAT_X_REFLECT_Y,

    { Reflect X and pad Y. }
    ReflectXPadY     = BL_EXTEND_MODE_REFLECT_X_PAD_Y,

    { Reflect X and repeat Y. }
    ReflectXRepeatY  = BL_EXTEND_MODE_REFLECT_X_REPEAT_Y);

type
  { Text encoding. }
  TBLTextEncoding = (
    { UTF-8 encoding. }
    UTF8   = BL_TEXT_ENCODING_UTF8,

    { UTF-16 encoding (native endian). }
    UTF16  = BL_TEXT_ENCODING_UTF16,

    { UTF-32 encoding (native endian). }
    UTF32  = BL_TEXT_ENCODING_UTF32,

    { LATIN1 encoding (one byte per character). }
    Latin1 = BL_TEXT_ENCODING_LATIN1);

{ ============================================================================
   [BLRange]
  ============================================================================ }

type
  { Provides start and end indexes. It's used to specify a range of an operation
    related to indexed containers like IBLPath, IBLGradient, etc... }
  TBLRange = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRange;
    function GetFinish: Integer; inline;
    function GetStart: Integer; inline;
    procedure SetFinish(const AValue: Integer); inline;
    procedure SetStart(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRange): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRange): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AStart, AFinish: Integer); overload; inline;
    function Equals(const AOther: TBLRange): Boolean; inline;

    property Start: Integer read GetStart write SetStart;
    property Finish: Integer read GetFinish write SetFinish;
  end;
  PBLRange = ^TBLRange;

function BLRange(const AStart, AFinish: Integer): TBLRange; inline;

{$ENDREGION 'General'}

{$REGION 'Array'}

{ ============================================================================
   [BLArrayView]
  ============================================================================ }

type
  { Array view }
  TBLArrayView<T> = record
  public type
    P = ^T;
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLArrayView;
    function GetLast: Pointer; inline;
    function GetLength: Integer; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AData: Pointer; const ALength: Integer); overload; inline;

    property Data: Pointer read FHandle.data;
    property First: Pointer read FHandle.data;
    property Last: Pointer read GetLast;
    property Length: Integer read GetLength;
  end;

{$ENDREGION 'Array'}

{$REGION 'Color'}

{ ============================================================================
   [BLRgba32]
  ============================================================================ }

type
  { 32-bit RGBA color (8-bit per component) stored as $AARRGGBB. }
  TBLRgba32 = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRgba32;
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Implicit(const AValue: Cardinal): TBLRgba32; inline; static;
    class operator Implicit(const AValue: TBLRgba32): Cardinal; inline; static;

    class operator Equal(const ALeft, ARight: TBLRgba32): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba32): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AValue: Cardinal); overload; inline;
    procedure Reset(const AR, AG, AB: Byte; const AA: Byte = 255); overload; inline;

    property R: Byte read FHandle.r write FHandle.r;
    property G: Byte read FHandle.g write FHandle.g;
    property B: Byte read FHandle.b write FHandle.b;
    property A: Byte read FHandle.a write FHandle.a;
    property Value: Cardinal read FHandle.value write FHandle.value;

    property IsOpaque: Boolean read GetIsOpaque;
    property IsTransparent: Boolean read GetIsTransparent;
  end;
  PBLRgba32 = ^TBLRgba32;

function BLRgba32: TBLRgba32; overload; inline;
function BLRgba32(const AValue: Cardinal): TBLRgba32; overload; inline;
function BLRgba32(const AR, AG, AB: Byte; const AA: Byte = 255): TBLRgba32; overload; inline;

{ ============================================================================
   [BLRgba64]
  ============================================================================ }

type
  { 64-bit RGBA color (16-bit per component) stored as $AAAARRRRGGGGBBBB. }
  TBLRgba64 = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRgba64;
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Implicit(const AValue: UInt64): TBLRgba64; inline; static;
    class operator Implicit(const AValue: TBLRgba64): UInt64; inline; static;

    class operator Equal(const ALeft, ARight: TBLRgba64): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba64): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AValue: UInt64); overload; inline;
    procedure Reset(const AR, AG, AB: Word; const AA: Word = $FFFF); overload; inline;
    procedure Reset(const AValue: TBLRgba32); overload; inline;

    property R: Word read FHandle.r write FHandle.r;
    property G: Word read FHandle.g write FHandle.g;
    property B: Word read FHandle.b write FHandle.b;
    property A: Word read FHandle.a write FHandle.a;
    property Value: UInt64 read FHandle.value write FHandle.value;

    property IsOpaque: Boolean read GetIsOpaque;
    property IsTransparent: Boolean read GetIsTransparent;
  end;
  PBLRgba64 = ^TBLRgba64;

function BLRgba64: TBLRgba64; overload; inline;
function BLRgba64(const AValue: UInt64): TBLRgba64; overload; inline;
function BLRgba64(const AR, AG, AB: Word; const AA: Word = $FFFF): TBLRgba64; overload; inline;
function BLRgba64(const AValue: TBLRgba32): TBLRgba64; overload; inline;

{ ============================================================================
   [BLRgba]
  ============================================================================ }

type
  { 128-bit RGBA color stored as 4 32-bit floating point values in [RGBA] order. }
  TBLRgba = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRgba;
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRgba): Boolean; inline; static;
    class operator Equal(const ALeft: TBLRgba; const ARight: TBLRgba32): Boolean; inline; static;
    class operator Equal(const ALeft: TBLRgba; const ARight: TBLRgba64): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLRgba; const ARight: TBLRgba32): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLRgba; const ARight: TBLRgba64): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AR, AG, AB: Single; const AA: Single = 1); overload; inline;
    procedure Reset(const ARgba: TBLRgba32); overload; inline;
    procedure Reset(const ARgba: TBLRgba64); overload; inline;

    property R: Single read FHandle.r write FHandle.r;
    property G: Single read FHandle.g write FHandle.g;
    property B: Single read FHandle.b write FHandle.b;
    property A: Single read FHandle.a write FHandle.a;

    property IsOpaque: Boolean read GetIsOpaque;
    property IsTransparent: Boolean read GetIsTransparent;
  end;
  PBLRgba = ^TBLRgba;

function BLRgba: TBLRgba; overload; inline;
function BLRgba(const AR, AG, AB: Single; const AA: Single = 1): TBLRgba; overload; inline;
function BLRgba(const ARgba: TBLRgba32): TBLRgba; overload; inline;
function BLRgba(const ARgba: TBLRgba64): TBLRgba; overload; inline;

type
  { Adds TBLRgba32 <-> TBLRgba64 <-> TBLRgba conversion }
  _TBLRgba32Helper = record helper for TBLRgba32
  public
    procedure Reset(const AValue: TBLRgba64); overload; inline;
    procedure Reset(const AValue: TBLRgba); overload; inline;
  end;

function BLRgba32(const AValue: TBLRgba64): TBLRgba32; overload; inline;
function BLRgba32(const AValue: TBLRgba): TBLRgba32; overload; inline;

type
  { Adds TBLRgba64 <-> TBLRgba conversion }
  _TBLRgba64Helper = record helper for TBLRgba64
  public
    procedure Reset(const AValue: TBLRgba); overload; inline;
  end;

function BLRgba64(const AValue: TBLRgba): TBLRgba32; overload; inline;

{$ENDREGION 'Color'}

{$REGION 'File System'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { File read flags used by IBLFileSystem.ReadFile. }
  TBLFileReadFlag = (
    { Use memory mapping to read the content of the file.

      The destination buffer would be configured to use the memory mapped buffer
      instead of allocating its own. }
    MmapEnabled = 0,

    { Avoid memory mapping of small files.

      The size of small file is determined by Blend2D, however, you should
      expect it to be 16kB or 64kB depending on host operating system. }
    MmapAvoidSmall = 1,

    { Do not fallback to regular read if memory mapping fails. It's worth noting
      that memory mapping would fail for files stored on filesystem that is not
      local (like a mounted network filesystem, etc...). }
    MmapNoFallback = 3);
  TBLFileReadFlags = set of TBLFileReadFlag;

{$ENDREGION 'File System'}

{$REGION 'Geometry'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Direction of a geometry used by geometric primitives and paths. }
  TBLGeometryDirection = (
    { No direction specified. }
    None = BL_GEOMETRY_DIRECTION_NONE,

    { Clockwise direction. }
    CW   = BL_GEOMETRY_DIRECTION_CW,

    { Counter-clockwise direction. }
    CCW  = BL_GEOMETRY_DIRECTION_CCW);

type
  { Geometry type.

    Geometry describes a shape or path that can be either rendered or added to
    a BLPath container. Both IBLPath and IBLContext provide functionality to
    work with all geometry types. Please note that each type provided here
    requires to pass a matching record to the function that consumes
    AGeometryType and AGeometryData arguments. }
  TBLGeometryType = (
    { No geometry provided. }
    None           = BL_GEOMETRY_TYPE_NONE,

    { TBLBoxI record. }
    BoxI           = BL_GEOMETRY_TYPE_BOXI,

    { TBLBox record. }
    BoxD           = BL_GEOMETRY_TYPE_BOXD,

    { TBLRectI record. }
    RectI          = BL_GEOMETRY_TYPE_RECTI,

    { TBLRect record. }
    RectD          = BL_GEOMETRY_TYPE_RECTD,

    { TBLCircle record. }
    Circle         = BL_GEOMETRY_TYPE_CIRCLE,

    { TBLEllipse record. }
    Ellipse        = BL_GEOMETRY_TYPE_ELLIPSE,

    { TBLRoundRect record. }
    RoundRect      = BL_GEOMETRY_TYPE_ROUND_RECT,

    { TBLArc record. }
    Arc            = BL_GEOMETRY_TYPE_ARC,

    { TBLArc record representing chord. }
    Chord          = BL_GEOMETRY_TYPE_CHORD,

    { TBLArc record representing pie. }
    Pie            = BL_GEOMETRY_TYPE_PIE,

    { TBLLine record. }
    Line           = BL_GEOMETRY_TYPE_LINE,

    { TBLTriangle record. }
    Triangle       = BL_GEOMETRY_TYPE_TRIANGLE,

    { TBLArrayView<TBLPointI> representing a polyline. }
    PolylineI      = BL_GEOMETRY_TYPE_POLYLINEI,

    { TBLArrayView<TBLPoint> representing a polyline. }
    PolylineD      = BL_GEOMETRY_TYPE_POLYLINED,

    { TBLArrayView<TBLPointI> representing a polygon. }
    PolygonI       = BL_GEOMETRY_TYPE_POLYGONI,

    { TBLArrayView<TBLPoint> representing a polygon. }
    PolygonD       = BL_GEOMETRY_TYPE_POLYGOND,

    { TBLArrayView<TBLBoxI> record. }
    ArrayViewBoxI  = BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI,

    { TBLArrayView<TBLBox> struct. }
    ArrayViewBoxD  = BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD,

    { TBLArrayView<TBLRectI> record. }
    ArrayviewRectI = BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI,

    { TBLArrayView<TBLRect> record. }
    ArrayViewRectD = BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD,

    { IBLPath. }
    Path           = BL_GEOMETRY_TYPE_PATH,

    { IBLRegion. }
    Region         = BL_GEOMETRY_TYPE_REGION,

    { The last simple type. }
    SimpleLast     = TBLGeometryType.Triangle);

type
  { Fill rule. }
  TBLFillRule = (
    { Non-zero fill-rule. }
    NonZero = BL_FILL_RULE_NON_ZERO,

    { Even-odd fill-rule. }
    EvenOdd = BL_FILL_RULE_EVEN_ODD);

type
  { Hit-test result. }
  TBLHitTest = (
    { Fully in. }
    FullyIn     = BL_HIT_TEST_IN,

    { Partially in/out. }
    PartiallyIn = BL_HIT_TEST_PART,

    { Fully out. }
    FullyOut    = BL_HIT_TEST_OUT,

    { Hit test failed (invalid argument, NaNs, etc). }
    Invalid     = BL_HIT_TEST_INVALID);

{ ============================================================================
   [BLPointI]
  ============================================================================ }

type
  { Point specified as [x, y] using Integer as a storage type. }
  TBLPointI = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPointI;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLPointI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLPointI): Boolean; inline; static;

    class operator Negative(const AValue: TBLPointI): TBLPointI; inline; static;

    class operator Add(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Add(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Add(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;

    class operator Subtract(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Subtract(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Subtract(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;

    class operator Multiply(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Multiply(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Multiply(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;

    class operator IntDivide(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator IntDivide(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator IntDivide(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX, AY: Integer); overload; inline;

    function Equals(const AOther: TBLPointI): Boolean; inline;

    property X: Integer read FHandle.x write FHandle.x;
    property Y: Integer read FHandle.y write FHandle.y;
  end;
  PBLPointI = ^TBLPointI;

function BLPointI: TBLPointI; overload; inline;
function BLPointI(const AX, AY: Integer): TBLPointI; overload; inline;

{ ============================================================================
   [SizeI]
  ============================================================================ }

type
  { Size specified as [w, h] using Integer as a storage type. }
  TBLSizeI = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLSizeI;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLSizeI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLSizeI): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AW, AH: Integer); overload; inline;

    function Equals(const AOther: TBLSizeI): Boolean; inline;

    property W: Integer read FHandle.w write FHandle.w;
    property H: Integer read FHandle.h write FHandle.h;
  end;
  PBLSizeI = ^TBLSizeI;

function BLSizeI: TBLSizeI; overload; inline;
function BLSizeI(const AW, AH: Integer): TBLSizeI; overload; inline;

{ ============================================================================
   [BLBoxI]
  ============================================================================ }

type
  { Box specified as [x0, y0, x1, y1] using Integer as a storage type. }
  TBLBoxI = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLBoxI;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
    procedure SetHeight(const AValue: Integer); inline;
    procedure SetWidth(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLBoxI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLBoxI): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Integer); overload; inline;

    function Equals(const AOther: TBLBoxI): Boolean; inline;
    function Contains(const AX, AY: Integer): Boolean; overload; inline;
    function Contains(const AP: TBLPointI): Boolean; overload; inline;

    property X0: Integer read FHandle.x0 write FHandle.x0;
    property Y0: Integer read FHandle.y0 write FHandle.y0;
    property X1: Integer read FHandle.x1 write FHandle.x1;
    property Y1: Integer read FHandle.y1 write FHandle.y1;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;
  PBLBoxI = ^TBLBoxI;

function BLBoxI: TBLBoxI; overload; inline;
function BLBoxI(const AX0, AY0, AX1, AY1: Integer): TBLBoxI; overload; inline;

{ ============================================================================
   [BLRectI]
  ============================================================================ }

type
  { Rectangle specified as [x, y, w, h] using Integer as a storage type. }
  TBLRectI = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRectI;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRectI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRectI): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX, AY, AW, AH: Integer); overload; inline;

    function Equals(const AOther: TBLRectI): Boolean; inline;

    property X: Integer read FHandle.x write FHandle.x;
    property Y: Integer read FHandle.y write FHandle.y;
    property W: Integer read FHandle.w write FHandle.w;
    property H: Integer read FHandle.h write FHandle.h;
  end;
  PBLRectI = ^TBLRectI;

function BLRectI: TBLRectI; overload; inline;
function BLRectI(const AX, AY, AW, AH: Integer): TBLRectI; overload; inline;

{ ============================================================================
   [BLPoint]
  ============================================================================ }

type
  { Point specified as [x, y] using Double as a storage type. }
  TBLPoint = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPoint;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLPoint): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLPoint): Boolean; inline; static;

    class operator Negative(const AValue: TBLPoint): TBLPoint; inline; static;

    class operator Add(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Add(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Add(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;

    class operator Subtract(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Subtract(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Subtract(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;

    class operator Multiply(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Multiply(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Multiply(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;

    class operator Divide(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Divide(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Divide(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX, AY: Double); overload; inline;

    function Equals(const AOther: TBLPoint): Boolean; inline;

    property X: Double read FHandle.x write FHandle.x;
    property Y: Double read FHandle.y write FHandle.y;
  end;
  PBLPoint = ^TBLPoint;

function BLPoint: TBLPoint; overload; inline;
function BLPoint(const AX, AY: Double): TBLPoint; overload; inline;

{ ============================================================================
   [Size]
  ============================================================================ }

type
  { Size specified as [w, h] using Double as a storage type. }
  TBLSize = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLSize;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLSize): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLSize): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AW, AH: Double); overload; inline;

    function Equals(const AOther: TBLSize): Boolean; inline;

    property W: Double read FHandle.w write FHandle.w;
    property H: Double read FHandle.h write FHandle.h;
  end;
  PBLSize = ^TBLSize;

function BLSize: TBLSize; overload; inline;
function BLSize(const AW, AH: Double): TBLSize; overload; inline;

{ ============================================================================
   [BLBox]
  ============================================================================ }

type
  { Box specified as [x0, y0, x1, y1] using Double as a storage type. }
  TBLBox = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLBox;
    function GetWidth: Double; inline;
    procedure SetWidth(const AValue: Double); inline;
    function GetHeight: Double; inline;
    procedure SetHeight(const AValue: Double); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLBox): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLBox): Boolean; inline; static;

    class operator Add(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Add(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Add(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Add(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;

    class operator Subtract(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Subtract(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Subtract(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Subtract(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;

    class operator Multiply(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Multiply(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Multiply(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Multiply(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;

    class operator Divide(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Divide(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Divide(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Divide(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;

    function Equals(const AOther: TBLBox): Boolean; inline;
    function Contains(const AX, AY: Double): Boolean; overload; inline;
    function Contains(const AP: TBLPoint): Boolean; overload; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property X1: Double read FHandle.x1 write FHandle.x1;
    property Y1: Double read FHandle.y1 write FHandle.y1;
    property Width: Double read GetWidth write SetWidth;
    property Height: Double read GetHeight write SetHeight;
  end;
  PBLBox = ^TBLBox;

function BLBox: TBLBox; overload; inline;
function BLBox(const AX0, AY0, AX1, AY1: Double): TBLBox; overload; inline;

{ ============================================================================
   [BLRect]
  ============================================================================ }

type
  { Rectangle specified as [x, y, w, h] using Double as a storage type. }
  TBLRect = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRect;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRect): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRect): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX, AY, AW, AH: Double); overload; inline;

    function Equals(const AOther: TBLRect): Boolean; inline;

    property X: Double read FHandle.x write FHandle.x;
    property Y: Double read FHandle.y write FHandle.y;
    property W: Double read FHandle.w write FHandle.w;
    property H: Double read FHandle.h write FHandle.h;
  end;
  PBLRect = ^TBLRect;

function BLRect: TBLRect; overload; inline;
function BLRect(const AX, AY, AW, AH: Double): TBLRect; overload; inline;

{ ============================================================================
   [BLLine]
  ============================================================================ }

type
  { Line specified as [x0, y0, x1, y1] using Double as a storage type. }
  TBLLine = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLLine;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLLine): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLLine): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;
    procedure Reset(const AP0, AP1: TBLPoint); overload; inline;

    function Equals(const AOther: TBLLine): Boolean; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property X1: Double read FHandle.x1 write FHandle.x1;
    property Y1: Double read FHandle.y1 write FHandle.y1;
  end;
  PBLLine = ^TBLLine;

function BLLine: TBLLine; overload; inline;
function BLLine(const AX0, AY0, AX1, AY1: Double): TBLLine; overload; inline;
function BLLine(const AP0, AP1: TBLPoint): TBLLine; overload; inline;

{ ============================================================================
   [BLTriangle]
  ============================================================================ }

type
  { Triangle data specified as [x0, y0, x1, y1, x2, y2] using Double as a
    storage type.}
  TBLTriangle = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLTriangle;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLTriangle): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLTriangle): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload; inline;

    function Equals(const AOther: TBLTriangle): Boolean; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property X1: Double read FHandle.x1 write FHandle.x1;
    property Y1: Double read FHandle.y1 write FHandle.y1;
    property X2: Double read FHandle.x2 write FHandle.x2;
    property Y2: Double read FHandle.y2 write FHandle.y2;
  end;
  PBLTriangle = ^TBLTriangle;

function BLTriangle: TBLTriangle; overload; inline;
function BLTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double): TBLTriangle; overload; inline;

{ ============================================================================
   [BLRoundRect]
  ============================================================================ }

type
  { Rounded rectangle specified as [x, y, w, h, rx, ry] using Double as a
    storage type. }
  TBLRoundRect = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRoundRect;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRoundRect): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRoundRect): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const ARect: TBLRect; const AR: Double); overload; inline;
    procedure Reset(const ARect: TBLRect; const ARX, ARY: Double); overload; inline;
    procedure Reset(const AX, AY, AW, AH, AR: Double); overload; inline;
    procedure Reset(const AX, AY, AW, AH, ARX, ARY: Double); overload; inline;

    function Equals(const AOther: TBLRoundRect): Boolean; inline;

    property X: Double read FHandle.x write FHandle.x;
    property Y: Double read FHandle.y write FHandle.y;
    property W: Double read FHandle.w write FHandle.w;
    property H: Double read FHandle.h write FHandle.h;
  end;
  PBLRoundRect = ^TBLRoundRect;

function BLRoundRect: TBLRoundRect; overload; inline;
function BLRoundRect(const ARect: TBLRect; const AR: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const ARect: TBLRect; const ARX, ARY: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const AX, AY, AW, AH, AR: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const AX, AY, AW, AH, ARX, ARY: Double): TBLRoundRect; overload; inline;

{ ============================================================================
   [BLCircle]
  ============================================================================ }

type
  { Circle specified as [cx, cy, r] using Double as a storage type. }
  TBLCircle = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLCircle;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLCircle): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLCircle): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, AR: Double); overload; inline;

    function Equals(const AOther: TBLCircle): Boolean; inline;

    property CX: Double read FHandle.cx write FHandle.cx;
    property CY: Double read FHandle.cy write FHandle.cy;
    property R: Double read FHandle.r write FHandle.r;
  end;
  PBLCircle = ^TBLCircle;

function BLCircle: TBLCircle; overload; inline;
function BLCircle(const ACX, ACY, AR: Double): TBLCircle; overload; inline;

{ ============================================================================
   [BLEllipse]
  ============================================================================ }

type
  { Ellipse specified as [cx, cy, rx, dy] using Double as a storage type. }
  TBLEllipse = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLEllipse;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLEllipse): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLEllipse): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, ARX, ARY: Double); overload; inline;

    function Equals(const AOther: TBLEllipse): Boolean; inline;

    property CX: Double read FHandle.cx write FHandle.cx;
    property CY: Double read FHandle.cy write FHandle.cy;
    property RX: Double read FHandle.rx write FHandle.rx;
    property RY: Double read FHandle.ry write FHandle.ry;
  end;
  PBLEllipse = ^TBLEllipse;

function BLEllipse: TBLEllipse; overload; inline;
function BLEllipse(const ACX, ACY, ARX, ARY: Double): TBLEllipse; overload; inline;

{ ============================================================================
   [BLArc]
  ============================================================================ }

type
  { Arc specified as [cx, cy, rx, ry, start, sweep] using Double as a storage
    type. }
  TBLArc = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLArc;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLArc): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLArc): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, AR, AStart, ASweep: Double); overload; inline;
    procedure Reset(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload; inline;

    function Equals(const AOther: TBLArc): Boolean; inline;

    property CX: Double read FHandle.cx write FHandle.cx;
    property CY: Double read FHandle.cy write FHandle.cy;
    property RX: Double read FHandle.rx write FHandle.rx;
    property RY: Double read FHandle.ry write FHandle.ry;
    property Start: Double read FHandle.start write FHandle.start;
    property Sweep: Double read FHandle.sweep write FHandle.sweep;
  end;
  PBLArc = ^TBLArc;

function BLArc: TBLArc; overload; inline;
function BLArc(const ACX, ACY, AR, AStart, ASweep: Double): TBLArc; overload; inline;
function BLArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double): TBLArc; overload; inline;

{ ============================================================================
   [Globals Functions]
  ============================================================================ }

function BLAbs(const AA: TBLPoint): TBLPoint; overload; inline;
function BLAbs(const AA: TBLSize): TBLSize; overload; inline;

function BLMin(const AA, AB: TBLPoint): TBLPoint; overload; inline;
function BLMin(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
function BLMin(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
function BLMin(const AA, AB: TBLSize): TBLSize; overload; inline;

function BLMax(const AA, AB: TBLPoint): TBLPoint; overload; inline;
function BLMax(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
function BLMax(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
function BLMax(const AA, AB: TBLSize): TBLSize; overload; inline;

function BLClamp(const AA: TBLPoint; const AB, AC: Double): TBLPoint; inline;

{$ENDREGION 'Geometry'}

{$REGION 'Matrix'}

{ ============================================================================
   [BLMatrix2D - Enums]
  ============================================================================ }

type
  { 2D matrix type that can be obtained by querying TBLMatrix2D.MatrixType.

    Identity  Transl.  Scale     Swap    Affine
     [1  0]   [1  0]   [.  0]   [0  .]   [.  .]
     [0  1]   [0  1]   [0  .]   [.  0]   [.  .]
     [0  0]   [.  .]   [.  .]   [.  .]   [.  .] }
  TBLMatrix2DType = (
    { Identity matrix. }
    Identity  = BL_MATRIX2D_TYPE_IDENTITY,

    { Has translation part (the rest is like identity). }
    Translate = BL_MATRIX2D_TYPE_TRANSLATE,

    { Has translation and scaling parts. }
    Scale     = BL_MATRIX2D_TYPE_SCALE,

    { Has translation and scaling parts, however scaling swaps X/Y. }
    Swap      = BL_MATRIX2D_TYPE_SWAP,

    { Generic affine matrix. }
    Affine    = BL_MATRIX2D_TYPE_AFFINE,

    { Invalid/degenerate matrix not useful for transformations. }
    Invalid   = BL_MATRIX2D_TYPE_INVALID);

type
  { 2D matrix data index. }
  TBLMatrix2DValue = (
    { Value at index 0 - M00. }
    M00 = BL_MATRIX2D_VALUE_00,

    { Value at index 1 - M01. }
    M01 = BL_MATRIX2D_VALUE_01,

    { Value at index 2 - M10. }
    M10 = BL_MATRIX2D_VALUE_10,

    { Value at index 3 - M11. }
    M11 = BL_MATRIX2D_VALUE_11,

    { Value at index 4 - M20. }
    M20 = BL_MATRIX2D_VALUE_20,

    { Value at index 5 - M21. }
    M21 = BL_MATRIX2D_VALUE_21);

{ ============================================================================
   [BLMatrix2D]
  ============================================================================ }

type
  { 2D matrix represents an affine transformation matrix that can be used to
    transform geometry and images. }
  TBLMatrix2D = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLMatrix2D;
    function GetElement(const AIndex: TBLMatrix2DValue): Double; inline;
    procedure SetElement(const AIndex: TBLMatrix2DValue; const AValue: Double); inline;
    function GetMatrixType: TBLMatrix2DType; inline;
    function GetDeterminant: Double; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLMatrix2D): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLMatrix2D): Boolean; inline; static;

    { Creates a new matrix initialized to identity. }
    class function MakeIdentity: TBLMatrix2D; inline; static;

    { Creates a new matrix initialized to translation. }
    class function MakeTranslation(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeTranslation(const AP: TBLPoint): TBLMatrix2D; overload; inline; static;
    class function MakeTranslation(const AP: TBLPointI): TBLMatrix2D; overload; inline; static;

    { Creates a new matrix initialized to scaling. }
    class function MakeScaling(const AXY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AP: TBLPoint): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AP: TBLPointI): TBLMatrix2D; overload; inline; static;

    { Creates a new matrix initialized to rotation, optional around point. }
    class function MakeRotation(const AAngle: Double): TBLMatrix2D; overload; inline; static;
    class function MakeRotation(const AAngle, AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeRotation(const AAngle: Double; const AP: TBLPoint): TBLMatrix2D; overload; inline; static;

    { Create a new skewing matrix. }
    class function MakeSkewing(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeSkewing(const AP: TBLPoint): TBLMatrix2D; overload; inline; static;

    { Create a new rotation matrix specified by ASin and ACos and optional
      translation. }
    class function MakeSinCos(const ASin, ACos: Double; const ATranslateX: Double = 0;
      const ATranslateY: Double = 0): TBLMatrix2D; overload; inline; static;
    class function MakeSinCos(const ASin, ACos: Double; const ATranslate: TBLPoint): TBLMatrix2D; overload; inline; static;

    { Inverts ASrc matrix and stores the result in ADst.
      Returns True if the matrix has been inverted successfully. }
    class function Invert(const ASrc: TBLMatrix2D; out ADst: TBLMatrix2D): Boolean; overload; inline; static;
  public
    { Resets matrix to identity. }
    procedure Reset; overload; inline;

    { Creates a new matrix initialized to:
        [m00 m01]
        [m10 m11]
        [m20 m21] }
    procedure Reset(const AM00, AM01, AM10, AM11, AM20, AM21: Double); overload; inline;

    { Resets matrix to translation. }
    procedure ResetToTranslation(const AX, AY: Double); overload; inline;
    procedure ResetToTranslation(const AP: TBLPoint); overload; inline;
    procedure ResetToTranslation(const AP: TBLPointI); overload; inline;

    { Resets matrix to scaling. }
    procedure ResetToScaling(const AXY: Double); overload; inline;
    procedure ResetToScaling(const AX, AY: Double); overload; inline;
    procedure ResetToScaling(const AP: TBLPoint); overload; inline;
    procedure ResetToScaling(const AP: TBLPointI); overload; inline;

    { Resets matrix to rotation, optional around point. }
    procedure ResetToRotation(const AAngle: Double); overload;
    procedure ResetToRotation(const AAngle, AX, AY: Double); overload;
    procedure ResetToRotation(const AAngle: Double; const AP: TBLPoint); overload;

    { Resets matrix to skewing. }
    procedure ResetToSkewing(const AX, AY: Double); overload;
    procedure ResetToSkewing(const AP: TBLPoint); overload;

    { Resets matrix to rotation specified by ASin and ACos and optional
      translation. }
    procedure ResetToSinCos(const ASin, ACos: Double; const ATranslateX: Double = 0;
      const ATranslateY: Double = 0); overload; inline;
    procedure ResetToSinCos(const ASin, ACos: Double; const ATranslate: TBLPoint); overload; inline;

    function Equals(const AOther: TBLMatrix2D): Boolean; inline;

    procedure Translate(const AX, AY: Double); overload; inline;
    procedure Translate(const AP: TBLPoint); overload; inline;
    procedure Translate(const AP: TBLPointI); overload; inline;

    procedure Scale(const AXY: Double); overload; inline;
    procedure Scale(const AX, AY: Double); overload; inline;
    procedure Scale(const AP: TBLPoint); overload; inline;
    procedure Scale(const AP: TBLPointI); overload; inline;

    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload;

    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const AP: TBLPoint); overload;

    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload; inline;
    procedure PostTranslate(const AP: TBLPoint); overload; inline;
    procedure PostTranslate(const AP: TBLPointI); overload; inline;

    procedure PostScale(const AXY: Double); overload; inline;
    procedure PostScale(const AX, AY: Double); overload; inline;
    procedure PostScale(const AP: TBLPoint); overload; inline;
    procedure PostScale(const AP: TBLPointI); overload; inline;

    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload;

    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const AP: TBLPoint); overload;

    procedure PostTransform(const AMatrix: TBLMatrix2D);

    { Inverts the matrix, returns True if the matrix has been inverted
      successfully. }
    function Invert: Boolean; overload;

    function MapPoint(const AX, AY: Double): TBLPoint; overload; inline;
    function MapPoint(const AP: TBLPoint): TBLPoint; overload; inline;

    function MapVector(const AX, AY: Double): TBLPoint; overload; inline;
    function MapVector(const AV: TBLPoint): TBLPoint; overload; inline;

    property Elements[const AIndex: TBLMatrix2DValue]: Double read GetElement write SetElement; default;
    property M00: Double read FHandle.m00 write FHandle.m00;
    property M01: Double read FHandle.m01 write FHandle.m01;
    property M10: Double read FHandle.m10 write FHandle.m10;
    property M11: Double read FHandle.m11 write FHandle.m11;
    property M20: Double read FHandle.m20 write FHandle.m20;
    property M21: Double read FHandle.m21 write FHandle.m21;

    { The matrix type }
    property MatrixType: TBLMatrix2DType read GetMatrixType;

    { Calculates the matrix determinant. }
    property Determinant: Double read GetDeterminant;
  end;
  PBLMatrix2D = ^TBLMatrix2D;

function BLMatrix2D: TBLMatrix2D; overload; inline;
function BLMatrix2D(const AM00, AM01, AM10, AM11, AM20, AM21: Double): TBLMatrix2D; overload; inline;

{$ENDREGION 'Matrix'}

{$REGION 'Random'}

{ ============================================================================
   [BLRandom]
  ============================================================================ }

type
  { Simple pseudo random number generator.

    The current implementation uses a PRNG called `XORSHIFT+`, which has 64-bit
    seed, 128 bits of state, and full period `2^128 - 1`.

    Based on a paper by Sebastiano Vigna:
      http://vigna.di.unimi.it/ftp/papers/xorshiftplus.pdf }
  TBLRandom = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRandom;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLRandom): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRandom): Boolean; inline; static;
  public
    { Resets the random number generator to the given ASeed. }
    procedure Reset(const ASeed: UInt64 = 0);

    { Tests whether the random number generator is equivalent to AOther.

      Random number generator would only be equivalent to AOther if it was
      initialized from the same seed and has the same internal state. }
    function Equals(const AOther: TBLRandom): Boolean; inline;

    { Returns the next pseudo-random UInt64 value and advances its state. }
    function NextUInt64: UInt64;

    { Returns the next pseudo-random UInt32 value and advances its state. }
    function NextUInt32: UInt32;

    { Returns the next pseudo-random Double precision floating point in [0..1)
      range and advances its state. }
    function NextDouble: Double;
  end;
  PBLRandom = ^TBLRandom;

{$ENDREGION 'Random'}

{$REGION 'Gradient'}

{ ============================================================================
   [BLGradient - Enums]
  ============================================================================ }

type
  { Gradient type. }
  TBLGradientType = (
    { Linear gradient type. }
    Linear  = BL_GRADIENT_TYPE_LINEAR,

    { Radial gradient type. }
    Radial  = BL_GRADIENT_TYPE_RADIAL,

    { Conical gradient type. }
    Conical = BL_GRADIENT_TYPE_CONICAL);

type
  { Gradient data index. }
  TBLGradientValue = (
    { x0 - start 'x' for Linear/Radial and center 'x' for Conical. }
    CommonX0     = BL_GRADIENT_VALUE_COMMON_X0,

    { y0 - start 'y' for Linear/Radial and center 'y' for Conical. }
    CommonY0     = BL_GRADIENT_VALUE_COMMON_Y0,

    { x1 - end 'x' for Linear/Radial. }
    CommonX1     = BL_GRADIENT_VALUE_COMMON_X1,

    { y1 - end 'y' for Linear/Radial. }
    CommonY1     = BL_GRADIENT_VALUE_COMMON_Y1,

    { Radial gradient r0 radius. }
    RadialR0     = BL_GRADIENT_VALUE_RADIAL_R0,

    {Conical gradient angle. }
    ConicalAngle = BL_GRADIENT_VALUE_CONICAL_ANGLE);

{ ============================================================================
   [BLGradientStop]
  ============================================================================ }

type
  { Defines an Offset and Rgba color that us used by IBLGradient to define
    a linear transition between colors. }
  TBLGradientStop = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGradientStop;
    function GetRgba: TBLRgba64; inline;
    procedure SetRgba(const AValue: TBLRgba64); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLGradientStop): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLGradientStop): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AOffset: Double; const ARgba: TBLRgba32); overload; inline;
    procedure Reset(const AOffset: Double; const ARgba: TBLRgba64); overload; inline;
    function Equals(const AOther: TBLGradientStop): Boolean; inline;

    property Offset: Double read FHandle.offset write FHandle.offset;
    property Rgba: TBLRgba64 read GetRgba write SetRgba;
  end;
  PBLGradientStop = ^TBLGradientStop;

function BLGradientStop(const AOffset: Double; const ARgba: TBLRgba32): TBLGradientStop; overload; inline;
function BLGradientStop(const AOffset: Double; const ARgba: TBLRgba64): TBLGradientStop; overload; inline;

{ ============================================================================
   [BLLinearGradientValues]
  ============================================================================ }

type
  { Linear gradient values packed into a record. }
  TBLLinearGradientValues = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLLinearGradientValues;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property X1: Double read FHandle.x1 write FHandle.x1;
    property Y1: Double read FHandle.y1 write FHandle.y1;
  end;
  PBLLinearGradientValues = ^TBLLinearGradientValues;

function BLLinearGradientValues(const AX0, AY0, AX1, AY1: Double): TBLLinearGradientValues; inline;

{ ============================================================================
   [BLRadialGradientValues]
  ============================================================================ }

type
  { Radial gradient values packed into a record. }
  TBLRadialGradientValues = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRadialGradientValues;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1, AR0: Double); overload; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property X1: Double read FHandle.x1 write FHandle.x1;
    property Y1: Double read FHandle.y1 write FHandle.y1;
    property R0: Double read FHandle.r0 write FHandle.r0;
  end;
  PBLRadialGradientValues = ^TBLRadialGradientValues;

function BLRadialGradientValues(const AX0, AY0, AX1, AY1, AR0: Double): TBLRadialGradientValues; inline;

{ ============================================================================
   [BLConicalGradientValues]
  ============================================================================ }

type
  { Conical gradient values packed into a record. }
  TBLConicalGradientValues = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLConicalGradientValues;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AAngle: Double); overload; inline;

    property X0: Double read FHandle.x0 write FHandle.x0;
    property Y0: Double read FHandle.y0 write FHandle.y0;
    property Angle: Double read FHandle.angle write FHandle.angle;
  end;
  PBLConicalGradientValues = ^TBLConicalGradientValues;

function BLConicalGradientValues(const AX0, AY0, AAngle: Double): TBLConicalGradientValues; inline;

{ ============================================================================
   [BLGradient]
  ============================================================================ }

type
  { Gradient }
  IBLGradient = interface
  ['{804D20E0-8F67-415C-BD26-E56DC3BA4DDC}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetGradientType: TBLGradientType;
    procedure SetGradientType(const AValue: TBLGradientType);
    function GetExtendMode: TBLExtendMode;
    procedure SetExtendMode(const AValue: TBLExtendMode);
    function GetValue(const AIndex: TBLGradientValue): Double;
    procedure SetValue(const AIndex: TBLGradientValue; const AValue: Double);
    function GetLinear: TBLLinearGradientValues;
    procedure SetLinear(const AValue: TBLLinearGradientValues);
    function GetRadial: TBLRadialGradientValues;
    procedure SetRadial(const AValue: TBLRadialGradientValues);
    function GetConical: TBLConicalGradientValues;
    procedure SetConical(const AValue: TBLConicalGradientValues);
    function GetX0: Double;
    procedure SetX0(const AValue: Double);
    function GetY0: Double;
    procedure SetY0(const AValue: Double);
    function GetX1: Double;
    procedure SetX1(const AValue: Double);
    function GetY1: Double;
    procedure SetY1(const AValue: Double);
    function GetR0: Double;
    procedure SetR0(const AValue: Double);
    function GetAngle: Double;
    procedure SetAngle(const AValue: Double);
    function GetSize: Integer;
    function GetCapacity: Integer;
    function GetAllStops: PBLGradientStop;
    function GetStop(const AIndex: Integer): TBLGradientStop;
    procedure SetStop(const AIndex: Integer; const AValue: TBLGradientStop);
    function GetHasMatrix: Boolean;
    function GetMatrixType: TBLMatrix2DType;
    function GetMatrix: TBLMatrix2D;
    procedure SetMatrix(const AValue: TBLMatrix2D);
    function GetHandle: PBLGradientCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Initialize(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    procedure Initialize(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    procedure Initialize(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;

    procedure Initialize(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;

    procedure Reset;
    function Equals(const AOther: IBLGradient): Boolean;

    { Resets the gradient extend mode to TBLExtendMode.Pad. }
    procedure ResetExtendMode;

    procedure SetValues(const AIndex: TBLGradientValue; const AValues: TArray<Double>); overload;
    procedure SetValues(const AValues: TBLLinearGradientValues); overload;
    procedure SetValues(const AValues: TBLRadialGradientValues); overload;
    procedure SetValues(const AValues: TBLConicalGradientValues); overload;

    { Reserves the capacity of gradient for at least ACount stops. }
    procedure Reserve(const ACount: Integer);

    { Shrinks the capacity of gradient stops to fit the current usage. }
    procedure Shrink;

    procedure ResetStops;
    procedure SetStops(const AStops: TArray<TBLGradientStop>);
    procedure AddStop(const AOffset: Double; const ARgba: TBLRgba32); overload;
    procedure AddStop(const AOffset: Double; const ARgba: TBLRgba64); overload;
    procedure RemoveStop(const AIndex: Integer);
    procedure RemoveStopByOffset(const AOffset: Double; const AAll: Boolean = True);
    procedure RemoveStops(const ARange: TBLRange);
    procedure RemoveStopsByOffset(const AOffsetMin, AOffsetMax: Double);
    procedure ReplaceStop(const AIndex: Integer; const AOffset: Double;
      const ARgba: TBLRgba32); overload;
    procedure ReplaceStop(const AIndex: Integer; const AOffset: Double;
      const ARgba: TBLRgba64); overload;
    function IndexOfStop(const AOffset: Double): Integer;

    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const AP: TBLPoint); overload;
    procedure Translate(const AP: TBLPointI); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const AP: TBLPoint); overload;
    procedure Scale(const AP: TBLPointI); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const AP: TBLPoint); overload;
    procedure PostTranslate(const AP: TBLPointI); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const AP: TBLPoint); overload;
    procedure PostScale(const AP: TBLPointI); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);

    { Tests whether the gradient path is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the gradient is empty.
      Empty gradient is considered any gradient that has no stops. }
    property IsEmpty: Boolean read GetIsEmpty;

    { The type of the gradient }
    property GradientType: TBLGradientType read GetGradientType write SetGradientType;

    { The gradient extend mode }
    property ExtendMode: TBLExtendMode read GetExtendMode write SetExtendMode;

    property Values[const AIndex: TBLGradientValue]: Double read GetValue write SetValue;

    property Linear: TBLLinearGradientValues read GetLinear write SetLinear;
    property Radial: TBLRadialGradientValues read GetRadial write SetRadial;
    property Conical: TBLConicalGradientValues read GetConical write SetConical;

    property X0: Double read GetX0 write SetX0;
    property Y0: Double read GetY0 write SetY0;
    property X1: Double read GetX1 write SetX1;
    property Y1: Double read GetY1 write SetY1;
    property R0: Double read GetR0 write SetR0;
    property Angle: Double read GetAngle write SetAngle;

    { The number of stops the gradient has. }
    property Size: Integer read GetSize;

    { The gradient capacity [in stops]. }
    property Capacity: Integer read GetCapacity;

    { The gradient stop data. }
    property AllStops: PBLGradientStop read GetAllStops;

    { A gradient stop at AIndex. }
    property Stops[const AIndex: Integer]: TBLGradientStop read GetStop write SetStop;

    property HasMatrix: Boolean read GetHasMatrix;
    property MatrixType: TBLMatrix2DType read GetMatrixType;
    property Matrix: TBLMatrix2D read GetMatrix write SetMatrix;

    { Internal handle for use with the C API }
    property Handle: PBLGradientCore read GetHandle;
  end;

type
  { Implements IBLGradient }
  TBLGradient = class(TInterfacedObject, IBLGradient)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGradientCore;
    FIsReference: Boolean;
  protected
    { IBLGradient }
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetGradientType: TBLGradientType;
    procedure SetGradientType(const AValue: TBLGradientType);
    function GetExtendMode: TBLExtendMode;
    procedure SetExtendMode(const AValue: TBLExtendMode);
    function GetValue(const AIndex: TBLGradientValue): Double;
    procedure SetValue(const AIndex: TBLGradientValue; const AValue: Double);
    function GetLinear: TBLLinearGradientValues;
    procedure SetLinear(const AValue: TBLLinearGradientValues);
    function GetRadial: TBLRadialGradientValues;
    procedure SetRadial(const AValue: TBLRadialGradientValues);
    function GetConical: TBLConicalGradientValues;
    procedure SetConical(const AValue: TBLConicalGradientValues);
    function GetX0: Double;
    procedure SetX0(const AValue: Double);
    function GetY0: Double;
    procedure SetY0(const AValue: Double);
    function GetX1: Double;
    procedure SetX1(const AValue: Double);
    function GetY1: Double;
    procedure SetY1(const AValue: Double);
    function GetR0: Double;
    procedure SetR0(const AValue: Double);
    function GetAngle: Double;
    procedure SetAngle(const AValue: Double);
    function GetSize: Integer;
    function GetCapacity: Integer;
    function GetAllStops: PBLGradientStop;
    function GetStop(const AIndex: Integer): TBLGradientStop;
    procedure SetStop(const AIndex: Integer; const AValue: TBLGradientStop);
    function GetHasMatrix: Boolean;
    function GetMatrixType: TBLMatrix2DType;
    function GetMatrix: TBLMatrix2D;
    procedure SetMatrix(const AValue: TBLMatrix2D);
    function GetHandle: PBLGradientCore;

    procedure Initialize(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    procedure Initialize(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    procedure Initialize(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;

    procedure Initialize(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;

    procedure Reset;
    function Equals(const AOther: IBLGradient): Boolean; reintroduce; overload;

    procedure ResetExtendMode;

    procedure SetValues(const AIndex: TBLGradientValue; const AValues: TArray<Double>); overload;
    procedure SetValues(const AValues: TBLLinearGradientValues); overload;
    procedure SetValues(const AValues: TBLRadialGradientValues); overload;
    procedure SetValues(const AValues: TBLConicalGradientValues); overload;

    procedure Reserve(const ACount: Integer);

    procedure Shrink;

    procedure ResetStops;
    procedure SetStops(const AStops: TArray<TBLGradientStop>);
    procedure AddStop(const AOffset: Double; const ARgba: TBLRgba32); overload;
    procedure AddStop(const AOffset: Double; const ARgba: TBLRgba64); overload;
    procedure RemoveStop(const AIndex: Integer);
    procedure RemoveStopByOffset(const AOffset: Double; const AAll: Boolean = True);
    procedure RemoveStops(const ARange: TBLRange);
    procedure RemoveStopsByOffset(const AOffsetMin, AOffsetMax: Double);
    procedure ReplaceStop(const AIndex: Integer; const AOffset: Double;
      const ARgba: TBLRgba32); overload;
    procedure ReplaceStop(const AIndex: Integer; const AOffset: Double;
      const ARgba: TBLRgba64); overload;
    function IndexOfStop(const AOffset: Double): Integer;

    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const AP: TBLPoint); overload;
    procedure Translate(const AP: TBLPointI); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const AP: TBLPoint); overload;
    procedure Scale(const AP: TBLPointI); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const AP: TBLPoint); overload;
    procedure PostTranslate(const AP: TBLPointI); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const AP: TBLPoint); overload;
    procedure PostScale(const AP: TBLPointI); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);
  private
    constructor Create(const AHandle: BLGradientCore;
      const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    constructor Create(const AType: TBLGradientType;
      const AValues: PDouble = nil); overload;

    constructor Create(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    constructor Create(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;
    constructor Create(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode = TBLExtendMode.Pad;
      const AStops: TArray<TBLGradientStop> = nil); overload;

    constructor Create(const AValues: TBLLinearGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    constructor Create(const AValues: TBLRadialGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;
    constructor Create(const AValues: TBLConicalGradientValues;
      const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
      const AMatrix: TBLMatrix2D); overload;

    destructor Destroy; override;
    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{$ENDREGION 'Gradient'}

{$REGION 'Region'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Region type. }
  TBLRegionType = (
    { Region is empty (has no rectangles). }
    Empty   = BL_REGION_TYPE_EMPTY,

    { Region has one rectangle (rectangular). }
    Rect    = BL_REGION_TYPE_RECT,

    { Region has more YX sorted rectangles. }
    Complex = BL_REGION_TYPE_COMPLEX);

{ ============================================================================
   [BLRegion]
  ============================================================================ }

type
  TBLRegionView = TBLArrayView<TBLBoxI>;

type
  { 2D region.
    Region is a set of rectangles sorted and coalesced by their Y/X
    coordinates. }
  IBLRegion = interface
  ['{7AFAFA57-1358-490D-A984-4017EE9E3234}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetRegionType: TBLRegionType;
    function GetIsRect: Boolean;
    function GetIsComplex: Boolean;
    function GetSize: Integer;
    function GetCapacity: Integer;
    function GetData: PBLBoxI;
    function GetDataEnd: PBLBoxI;
    function GetBoundingBox: TBLBoxI;
    function GetView: TBLRegionView;
    function GetHandle: PBLRegionCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Clone: IBLRegion;

    { Tests whether this region and the AOther region are equal. }
    function Equals(const AOther: IBLRegion): Boolean;

    procedure Assign(const ABox: TBLBoxI); overload;
    procedure Assign(const ABoxes: TArray<TBLBoxI>); overload;
    procedure Assign(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    procedure Assign(const ARect: TBLRectI); overload;
    procedure Assign(const ARects: TArray<TBLRectI>); overload;
    procedure Assign(const ARects: PBLRectI; const ACount: Integer); overload;

    procedure Clear;

    { Reserves at least ACount boxes in this region. }
    procedure Reserve(const ACount: Integer);

    { Shrinks the region data so it consumes only memory it requires. }
    procedure Shrink;

    procedure Combine(const ARegion: IBLRegion; const ABooleanOp: TBLBooleanOp); overload;
    procedure Combine(const ABox: TBLBoxI; const ABooleanOp: TBLBooleanOp); overload;

    { Translates the region by the given point APt.

      Possible overflow will be handled by clipping to a maximum region
      boundary, so the final region could be smaller than the region before
      translation. }
    procedure Translate(const APt: TBLPointI);

    { Translates the region by the given point APt and clip it to the given
      AClipBox. }
    procedure TranslateAndClip(const APt: TBLPointI; const AClipBox: TBLBoxI); overload;

    { Intersects the region with AR and clip it to the given AClipBox. }
    procedure IntersectAndClip(const AR: IBLRegion; const AClipBox: TBLBoxI); overload;

    { Tests if a given point APt or ABox is in region }
    function HitTest(const APt: TBLPointI): TBLHitTest; overload;
    function HitTest(const ABox: TBLBoxI): TBLHitTest; overload;

    { Tests whether the region is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the region is empty. }
    property IsEmpty: Boolean read GetIsEmpty;

    { The type of the region }
    property RegionType: TBLRegionType read GetRegionType;

    { Tests whether the region is one rectangle. }
    property IsRect: Boolean read GetIsRect;

    { Tests whether the region is complex. }
    property IsComplex: Boolean read GetIsComplex;

    { The region size. }
    property Size: Integer read GetSize;

    { The region capacity. }
    property Capacity: Integer read GetCapacity;

    { Pointer to the region data. }
    property Data: PBLBoxI read GetData;

    { Pointer to the end of the region data. }
    property DataEnd: PBLBoxI read GetDataEnd;

    { The region's bounding box. }
    property BoundingBox: TBLBoxI read GetBoundingBox;

    { The region data as TBLRegionView. }
    property View: TBLRegionView read GetView;

    { Internal handle for use with the C API }
    property Handle: PBLRegionCore read GetHandle;
  end;

type
  { Implements IBLRegion }
  TBLRegion = class(TInterfacedObject, IBLRegion)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRegionCore;
  protected
    { IBLRegion }
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetRegionType: TBLRegionType;
    function GetIsRect: Boolean;
    function GetIsComplex: Boolean;
    function GetSize: Integer;
    function GetCapacity: Integer;
    function GetData: PBLBoxI;
    function GetDataEnd: PBLBoxI;
    function GetBoundingBox: TBLBoxI;
    function GetView: TBLRegionView;
    function GetHandle: PBLRegionCore;

    procedure Reset;
    function Clone: IBLRegion;

    function Equals(const AOther: IBLRegion): Boolean; reintroduce; overload;

    procedure Assign(const ABox: TBLBoxI); overload;
    procedure Assign(const ABoxes: TArray<TBLBoxI>); overload;
    procedure Assign(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    procedure Assign(const ARect: TBLRectI); overload;
    procedure Assign(const ARects: TArray<TBLRectI>); overload;
    procedure Assign(const ARects: PBLRectI; const ACount: Integer); overload;

    procedure Clear;

    procedure Reserve(const ACount: Integer);
    procedure Shrink;

    procedure Combine(const ARegion: IBLRegion; const ABooleanOp: TBLBooleanOp); overload;
    procedure Combine(const ABox: TBLBoxI; const ABooleanOp: TBLBooleanOp); overload;

    procedure Translate(const APt: TBLPointI); overload;
    procedure TranslateAndClip(const APt: TBLPointI; const AClipBox: TBLBoxI); overload;
    procedure IntersectAndClip(const AR: IBLRegion; const AClipBox: TBLBoxI); overload;

    function HitTest(const APt: TBLPointI): TBLHitTest; overload;
    function HitTest(const ABox: TBLBoxI): TBLHitTest; overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  public
    class function Combine(const AA, AB: IBLRegion; const ABooleanOp: TBLBooleanOp): IBLRegion; overload; static;
    class function Combine(const AA: IBLRegion; const AB: TBLBoxI; const ABooleanOp: TBLBooleanOp): IBLRegion; overload; static;
    class function Combine(const AA: TBLBoxI; const AB: IBLRegion; const ABooleanOp: TBLBooleanOp): IBLRegion; overload; static;
    class function Combine(const AA, AB: TBLBoxI; const ABooleanOp: TBLBooleanOp): IBLRegion; overload; static;

    class function Translate(const AR: IBLRegion; const APt: TBLPointI): IBLRegion; overload; static;
    class function TranslateAndClip(const AR: IBLRegion; const APt: TBLPointI;
      const AClipBox: TBLBoxI): IBLRegion; overload; static;
    class function IntersectAndClip(const AA, AB: IBLRegion;
      const AClipBox: TBLBoxI): IBLRegion; overload; static;
  end;

{$ENDREGION 'Region'}

{$REGION 'Path'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Path command. }
  TBLPathCmd = (
    { Move-to command (starts a new figure). }
    Move  = BL_PATH_CMD_MOVE,

    { On-path command (interpreted as line-to or the end of a curve). }
    &On   = BL_PATH_CMD_ON,

    { Quad-to control point. }
    Quad  = BL_PATH_CMD_QUAD,

    { Cubic-to control point (always used as a pair of commands). }
    Cubic = BL_PATH_CMD_CUBIC,

    { Close path. }
    Close = BL_PATH_CMD_CLOSE);
  PBLPathCmd = ^TBLPathCmd;

type
  { Path flags. }
  TBLPathFlag = (
    { Path is empty (no commands or close commands only). }
    Empty    = 0,

    { Path contains multiple figures. }
    Multiple = 1,

    { Path contains quad curves (at least one). }
    Quads    = 2,

    { Path contains cubic curves (at least one). }
    Cubics   = 3,

    { Path is invalid. }
    Invalid  = 30,

    { Flags are dirty (not reflecting the current status). }
    Dirty    = 31);
  TBLPathFlags = set of TBLPathFlag;

type
  { Path reversal mode. }
  TBLPathReverseMode = (
    { Reverse each figure and their order as well (default). }
    Complete = BL_PATH_REVERSE_MODE_COMPLETE,

    { Reverse each figure separately (keeps their order). }
    Separate = BL_PATH_REVERSE_MODE_SEPARATE);

type
  { Stroke join type. }
  TBLStrokeJoin = (
    { Miter-join possibly clipped at MiterLimit [default]. }
    MiterClip  = BL_STROKE_JOIN_MITER_CLIP,

    { Miter-join or bevel-join depending on MiterLimit condition. }
    MiterBevel = BL_STROKE_JOIN_MITER_BEVEL,

    { Miter-join or round-join depending on MiterLimit condition. }
    MiterRound = BL_STROKE_JOIN_MITER_ROUND,

    { Bevel-join. }
    Bevel      = BL_STROKE_JOIN_BEVEL,

    { Round-join. }
    Round      = BL_STROKE_JOIN_ROUND);

type
  { Position of a stroke-cap. }
  TBLStrokeCapPosition = (
    { Start of the path. }
    Start  = BL_STROKE_CAP_POSITION_START,

    { End of the path. }
    Finish = BL_STROKE_CAP_POSITION_END);

type
  { A presentation attribute defining the shape to be used at the end of open
    subpaths. }
  TBLStrokeCap = (
    { Butt cap [default]. }
    Butt        = BL_STROKE_CAP_BUTT,

    { Square cap. }
    Square      = BL_STROKE_CAP_SQUARE,

    { Round cap. }
    Round       = BL_STROKE_CAP_ROUND,

    { Round cap reversed. }
    RoundRev    = BL_STROKE_CAP_ROUND_REV,

    { Triangle cap. }
    Triangle    = BL_STROKE_CAP_TRIANGLE,

    { Triangle cap reversed. }
    TriangleRev = BL_STROKE_CAP_TRIANGLE_REV);

type
  { Stroke transform order. }
  TBLStrokeTransformOrder = (
    { Transform after stroke  => Transform(Stroke(Input)) [default]. }
    After  = BL_STROKE_TRANSFORM_ORDER_AFTER,

    { Transform before stroke => Stroke(Transform(Input)). }
    Before = BL_STROKE_TRANSFORM_ORDER_BEFORE);

type
  { Mode that specifies how curves are approximated to line segments. }
  TBLFlattenMode = (
    { Use default mode (decided by Blend2D). }
    Default   = BL_FLATTEN_MODE_DEFAULT,

    { Recursive subdivision flattening. }
    Recursive = BL_FLATTEN_MODE_RECURSIVE);

type
  { Mode that specifies how to construct offset curves. }
  TBLOffsetMode = (
    { Use default mode (decided by Blend2D). }
    Default   = BL_OFFSET_MODE_DEFAULT,

    { Iterative offset construction. }
    Iterative = BL_OFFSET_MODE_ITERATIVE);

type
  { TODO : Implement once added to Blend2D }
  TBLFitFlag = (_);
  TBLFitFlags = set of TBLFitFlag;

{ ============================================================================
   [BLApproximationOptions]
  ============================================================================ }

type
  { Options used to describe how geometry is approximated.

    This struct cannot be simply zeroed and then passed to functions that accept
    approximation options. Use BLApproximationOptions.Default to setup defaults
    and then alter values you want to change. }
  TBLApproximationOptions = record
  {$REGION 'Internal Declarations'}
  private class var
    FDefault: BLApproximationOptions;
  private
    FHandle: BLApproximationOptions;
    function GetFlattenMode: TBLFlattenMode; inline;
    function GetOffsetMode: TBLOffsetMode; inline;
    procedure SetFlattenMode(const AValue: TBLFlattenMode); inline;
    procedure SetOffsetMode(const AValue: TBLOffsetMode); inline;
  private
    class function GetDefault: TBLApproximationOptions; static;
  public
    class constructor Create;
  {$ENDREGION 'Internal Declarations'}
  public
    { Specifies how curves are flattened }
    property FlattenMode: TBLFlattenMode read GetFlattenMode write SetFlattenMode;

    { Specifies how curves are offsetted (used by stroking) }
    property OffsetMode: TBLOffsetMode read GetOffsetMode write SetOffsetMode;

    { Tolerance used to flatten curves. }
    property FlattenTolerance: Double read FHandle.flattenTolerance write FHandle.flattenTolerance;

    { Tolerance used to approximatecubic curves qith quadratic curves. }
    property SimplifyTolerance: Double read FHandle.simplifyTolerance write FHandle.simplifyTolerance;

    { Curve offsetting parameter, exact meaning depends on OffsetMode. }
    property OffsetParameter: Double read FHandle.offsetParameter write FHandle.offsetParameter;

    { Default approximation options }
    class property Default: TBLApproximationOptions read GetDefault;
  end;
  PBLApproximationOptions = ^TBLApproximationOptions;

{ ============================================================================
   [BLStrokeOptions]
  ============================================================================ }

type
  { Stroke options. }
  TBLStrokeOptions = record
  {$REGION 'Internal Declarations'}
  private type
    TScope = class(TInterfacedObject)
    private
      FHandle: PBLStrokeOptionsCore;
    public
      constructor Create(const AHandle: PBLStrokeOptionsCore);
      destructor Destroy; override;
    end;
  private
    FHandle: BLStrokeOptionsCore;
    FScope: IInterface;
    function GetStartCap: TBLStrokeCap; inline;
    procedure SetStartCap(const AValue: TBLStrokeCap); inline;
    function GetEndCap: TBLStrokeCap; inline;
    procedure SetEndCap(const AValue: TBLStrokeCap); inline;
    function GetJoin: TBLStrokeJoin; inline;
    procedure SetJoin(const AValue: TBLStrokeJoin); inline;
    function GetTransformOrder: TBLStrokeTransformOrder; inline;
    procedure SetTransformOrder(const AValue: TBLStrokeTransformOrder); inline;
    function GetDashArray: TArray<Double>; inline;
    procedure SetDashArray(const AValue: TArray<Double>);
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset;
    procedure SetCaps(const ACap: TBLStrokeCap); overload; inline;
    procedure SetCaps(const AStartCap, AEndCap: TBLStrokeCap); overload; inline;

    property StartCap: TBLStrokeCap read GetStartCap write SetStartCap;
    property EndCap: TBLStrokeCap read GetEndCap write SetEndCap;
    property Join: TBLStrokeJoin read GetJoin write SetJoin;
    property TransformOrder: TBLStrokeTransformOrder read GetTransformOrder write SetTransformOrder;
    property Width: Double read FHandle.width write FHandle.width;
    property MiterLimit: Double read FHandle.miterLimit write FHandle.miterLimit;
    property DashOffset: Double read FHandle.dashOffset write FHandle.dashOffset;
    property DashArray: TArray<Double> read GetDashArray write SetDashArray;
  end;
  PBLStrokeOptions = ^TBLStrokeOptions;

{ ============================================================================
   [BLPath - View]
  ============================================================================ }

type
  { 2D path view provides pointers to vertex and command data along with their
    size. }
  TBLPathView = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPathView;
    function GetCommands: PBLPathCmd; inline;
    function GetVertices: PBLPoint; inline;
    function GetCount: Integer; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const ACommands: PBLPathCmd; const AVertices: PBLPoint;
      const ACount: Integer); overload; inline;

    property Commands: PBLPathCmd read GetCommands;
    property Vertices: PBLPoint read GetVertices;
    property Count: Integer read GetCount;
  end;
  PBLPathView = ^TBLPathView;

function BLPathView(const ACommands: PBLPathCmd; const AVertices: PBLPoint;
  const ACount: Integer): TBLPathView; inline;

{ ============================================================================
   [BLPath]
  ============================================================================ }

type
  { 2D vector path }
  IBLPath = interface
  ['{18B368F9-0E76-4C83-9B21-06CDB127430E}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
    function GetCapacity: Integer;
    function GetVertexData: PBLPoint;
    function GetVertexDataEnd: PBLPoint;
    function GetLastVertex: TBLPoint;
    function GetCommandData: PBLPathCmd;
    function GetCommandDataEnd: PBLPathCmd;
    function GetView: TBLPathView;
    function GetInfoFlags: TBLPathFlags;
    function GetControlBox: TBLBox;
    function GetBoundingBox: TBLBox;
    function GetFigureRange(const AIndex: Integer): TBLRange;
    function GetHandle: PBLPathCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Clone: IBLPath;

    { Tests whether this path and the AOther path are equal.

      The equality check is deep. The data of both paths is examined and binary
      compared (thus a slight difference like -0 and +0 would make the equality
      check to fail). }
    function Equals(const AOther: IBLPath): Boolean;

    { Clears the content of the path. }
    procedure Clear;

    { Shrinks the capacity of the path to fit the current usage. }
    procedure Shrink;

    { Reserves the capacity of the path for at least ACoount vertices and
      commands. }
    procedure Reserve(const ACount: Integer);

    { Sets vertex at AIndex to ACmd and APt.
      Set APreserve to True to preserve the current command.}
    procedure SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
      const APt: TBLPoint; const APreserve: Boolean = False); overload;

    { Sets vertex at AIndex to ACmd and [AX, AY].
      Set APreserve to True to preserve the current command.}
    procedure SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
      const AX, AY: Double; const APreserve: Boolean = False); overload;

    { Moves to AP0.
      Appends TBLPathCmd.Move[AP0] command to the path. }
    procedure MoveTo(const AP0: TBLPoint); overload;

    { Moves to [AX0, AY0].
      Appends TBLPathCmd.Move[AX0, AY0] command to the path. }
    procedure MoveTo(const AX0, AY0: Double); overload;

    { Adds line to AP1.
      Appends TBLPathCmd.On[AP1] command to the path. }
    procedure LineTo(const AP1: TBLPoint); overload;

    { Adds line to [AX1, AY1].
      Appends TBLPathCmd.On[AX1, AY1] command to the path. }
    procedure LineTo(const AX1, AY1: Double); overload;

    { Adds a polyline (LineTo) of the given APoly array.
      Appends multiple TBLPathCmd.On[X, Y] commands to the path depending. }
    procedure PolyTo(const APoly: TArray<TBLPoint>); overload;
    procedure PolyTo(const APoly: PBLPoint; const ACount: Integer); overload;

    { Adds a quadratic curve to AP1 and AP2.
      Appends the following commands to the path:
      * TBLPathCmd.Quad[AP1]
      * TBLPathCmd.On[AP2]

      Matches SVG 'Q' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands }
    procedure QuadTo(const AP1, AP2: TBLPoint); overload;

    { Adds a quadratic curve to [AX1, AY1] and [AX2, AY2].
      Appends the following commands to the path:
      * TBLPathCmd.Quad[AX1, AY1]
      * TBLPathCmd.On[AX2, AY2]

      Matches SVG 'Q' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands }
    procedure QuadTo(const AX1, AY1, AX2, AY2: Double); overload;

    { Adds a cubic curve to AP1, AP2 and AP3.
      Appends the following commands to the path:
      * TBLPathCmd.Cubic[AP1]
      * TBLPathCmd.Cubic[AP2]
      * TBLPathCmd.On[AP3]

      Matches SVG 'C' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands }
    procedure CubicTo(const AP1, AP2, AP3: TBLPoint); overload;

    { Adds a cubic curve to [AX1, AY1], [AX2, AY2] and [AX3, AY3].
      Appends the following commands to the path:
      * TBLPathCmd.Cubic[AX1, AY1]
      * TBLPathCmd.Cubic[AX2, AY2]
      * TBLPathCmd.On[AX3, AY3]

      Matches SVG 'C' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands }
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double); overload;

    { Adds a smooth quadratic curve to AP2, calculating AP1 from last points.
      Appends the following commands to the path:
      * TBLPathCmd.Quad[calculated]
      * TBLPathCmd.On[AP2]

      Matches SVG 'T' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands }
    procedure SmoothQuadTo(const AP2: TBLPoint); overload;

    { Adds a smooth quadratic curve to [AX2, AY2], calculating [AX1, AY1] from
      last points.
      Appends the following commands to the path:
      * TBLPathCmd.Quad[calculated]
      * TBLPathCmd.On[AX2, AY2]

      Matches SVG 'T' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands }
    procedure SmoothQuadTo(const AX2, AY2: Double); overload;

    { Adds a smooth cubic curve to AP2 and AP3, calculating AP1 from last
      points.
      Appends the following commands to the path:
      * TBLPathCmd.Cubic[calculated]
      * TBLPathCmd.Cubic[AP2]
      * TBLPathCmd.On[AP3]

      Matches SVG 'S' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands }
    procedure SmoothCubicTo(const AP2, AP3: TBLPoint); overload;

    { Adds a smooth cubic curve to [AX2, AY2] and [AX3, AY3], calculating
      [AX1, AY1] from last points.
      Appends the following commands to the path:
      * TBLPathCmd.Cubic[calculated]
      * TBLPathCmd.Cubic[[AX2, AY2]]
      * TBLPathCmd.On[[AX3, AY3]]

      Matches SVG 'S' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands }
    procedure SmoothCubicTo(const AX2, AY2, AX3, AY3: Double); overload;

    { Adds an arc to the path.

      The center of the arc is specified by AC and radius by AR. Both AStart
      and ASweep angles are in radians. If the last vertex doesn't match the
      start of the arc then a LineTo would be emitted before adding the arc.
      Pass True in AForceMoveTo to always emit MoveTo at the beginning of the
      arc, which starts a new figure. }
    procedure ArcTo(const AC, AR: TBLPoint; const AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload;
    procedure ArcTo(const ACX, ACY, ARX, ARY, AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload;

    { Adds an arc quadrant (90deg) to the path. The first point AP1 specifies
      the quadrant corner and the last point AP2 specifies the end point. }
    procedure ArcQuadrantTo(const AP1, AP2: TBLPoint); overload;
    procedure ArcQuadrantTo(const AX1, AY1, AX2, AY2: Double); overload;

    { Adds an elliptic arc to the path that follows the SVG specification.

      Matches SVG 'A' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands }
    procedure EllipticArcTo(const ARP: TBLPoint; const AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AP1: TBLPoint); overload;
    procedure EllipticArcTo(const ARX, ARY, AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AX1, AY1: Double); overload;

    { Closes the current figure.
      Appends TBLPathCmd.Close to the path.

      Matches SVG 'Z' path command:
      https://www.w3.org/TR/SVG/paths.html#PathDataClosePathCommand }
    procedure Close;

    { Adds a closed rectangle to the path specified by ABox. }
    procedure AddBox(const ABox: TBLBoxI;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBox(const ABox: TBLBox;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed rectangle to the path specified by [AX0, AY0, AX1, AY1]. }
    procedure AddBox(const AX0, AY0, AX1, AY1: Double;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed rectangle to the path specified by ARect. }
    procedure AddRect(const ARect: TBLRectI;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRect(const ARect: TBLRect;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed rectangle to the path specified by [AX, AY, AW, AH]. }
    procedure AddRect(const AX, AY, AW, AH: Double;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a geometry to the path. }
    procedure AddGeometry(const AGeometryType: TBLGeometryType;
      const AGeometryData: Pointer; const AMatrix: PBLMatrix2D = nil;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW);

    { Adds a closed circle to the path. }
    procedure AddCircle(const ACircle: TBLCircle;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddCircle(const ACircle: TBLCircle; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed ellipse to the path. }
    procedure AddEllipse(const AEllipse: TBLEllipse;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddEllipse(const AEllipse: TBLEllipse; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed rounded ractangle to the path. }
    procedure AddRoundRect(const ARoundRect: TBLRoundRect;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: TBLRoundRect; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds an open arc to the path. }
    procedure AddArc(const AArc: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddArc(const AArc: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed chord to the path. }
    procedure AddChord(const AChord: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddChord(const AChord: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed pie to the path. }
    procedure AddPie(const APie: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPie(const APie: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds an open line to the path. }
    procedure AddLine(const ALine: TBLLine;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddLine(const ALine: TBLLine; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed triangle. }
    procedure AddTriangle(const ATriangle: TBLTriangle;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddTriangle(const ATriangle: TBLTriangle; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a polyline. }
    procedure AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a polygon. }
    procedure AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds an array of closed boxes. }
    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds an array of closed rectangles. }
    procedure AddRectArray(const ARects: TBLArrayView<TBLRectI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: TBLArrayView<TBLRectI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRectI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRectI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRectArray(const ARects: TBLArrayView<TBLRect>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: TBLArrayView<TBLRect>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRect; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRect; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds a closed region (converted to set of rectangles). }
    procedure AddRegion(const ARegion: IBLRegion;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRegion(const ARegion: IBLRegion; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    { Adds other APath to this path. }
    procedure AddPath(const APath: IBLPath); overload;

    { Adds other APath sliced by the given ARange to this path. }
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange); overload;

    { Adds other APath translated by ATranslate to this path. }
    procedure AddPath(const APath: IBLPath; const ATranslate: TBLPoint); overload;

    { Adds other APath translated by ATranslate and sliced by the given ARange
      to this path. }
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange;
      const ATranslate: TBLPoint); overload;

    { Adds other APath transformed by AMatrix to this path. }
    procedure AddPath(const APath: IBLPath; const AMatrix: TBLMatrix2D); overload;

    { Adds other APath transformed by AMatrix and sliced by the given ARange to
      this path. }
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange;
      const AMatrix: TBLMatrix2D); overload;

    { Adds other APath, but reversed. }
    procedure AddReversedPath(const APath: IBLPath;
      const AReverseMode: TBLPathReverseMode); overload;
    procedure AddReversedPath(const APath: IBLPath; const ARange: TBLRange;
      const AReverseMode: TBLPathReverseMode); overload;

    { Adds a stroke of APath to this path. }
    procedure AddStrokedPath(const APath: IBLPath;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload;
    procedure AddStrokedPath(const APath: IBLPath; const ARange: TBLRange;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload;

    { Manipulation }
    procedure RemoveRange(const ARange: TBLRange);

    { Translates the whole path by AP. }
    procedure Translate(const AP: TBLPoint); overload;

    { Translates a part of the path specified by the given ARange by AP. }
    procedure Translate(const ARange: TBLRange; const AP: TBLPoint); overload;

    { Transforms the whole path by AMatrix. }
    procedure Transform(const AMatrix: TBLMatrix2D); overload;

    { Transforms a part of the path specified by the given ARange by AMatrix. }
    procedure Transform(const ARange: TBLRange; const AMatrix: TBLMatrix2D); overload;

    { Fits the whole path into the given ARect by taking into account fit flags
      passed by AFitFlags. }
    procedure FitTo(const ARect: TBLRect; const AFitFlags: TBLFitFlags = []); overload;

    { Fits a part of the path specified by the given ARange into the given ARect
      by taking into account fit flags passed by AFitFlags. }
    procedure FitTo(const ARange: TBLRange; const ARect: TBLRect;
      const AFitFlags: TBLFitFlags = []); overload;

    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double): Integer; overload;
    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double;
      out AActualDistance: Double): Integer; overload;

    { Hit tests the given point AP by respecting the given AFillRule. }
    function HitTest(const AP: TBLPoint; const AFillRule: TBLFillRule): TBLHitTest;

    { Tests whether the 2D path is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the 2D path is empty (its size equals zero). }
    property IsEmpty: Boolean read GetIsEmpty;

    { Count of vertices used }
    property Count: Integer read GetCount;

    { Path capacity (count of allocated vertices) }
    property Capacity: Integer read GetCapacity;

    { Path's vertex data (read-only). }
    property VertexData: PBLPoint read GetVertexData;

    { End of path's vertex data (read-only). }
    property VertexDataEnd: PBLPoint read GetVertexDataEnd;

    { The last vertex of the path. If the very last command of the path is
      TBLPathCmd.Close then the path will be iterated in reverse order to match
      the initial vertex of the last figure. }
    property LastVertex: TBLPoint read GetLastVertex;

    { Path's command data (read-only). }
    property CommandData: PBLPathCmd read GetCommandData;

    { End of path's command data (read-only). }
    property CommandDataEnd: PBLPathCmd read GetCommandDataEnd;

    { The path data as a read-only TBLPathView. }
    property View: TBLPathView read GetView;

    { Update a path information if necessary. }
    property InfoFlags: TBLPathFlags read GetInfoFlags;

    { The bounding box of all vertices and control points.

      Control box is simply bounds of all vertices the path has without further
      processing. It contains both on-path and off-path points. Consider using
      BoundingBox if you need a visual bounding box. }
    property ControlBox: TBLBox read GetControlBox;

    { The bounding box of all on-path vertices and curve extremas.

      The bounding box could be smaller than a bounding box obtained by
      ControlBox as it's calculated by merging only start/end points and curves
      at their extremas (not control points). The resulting bounding box
      represents a visual bounds of the path. }
    property BoundingBox: TBLBox read GetBoundingBox;

    { The range describing figures at the given AIndex. }
    property FigureRanges[const AIndex: Integer]: TBLRange read GetFigureRange;

    { Internal handle for use with the C API }
    property Handle: PBLPathCore read GetHandle;
  end;

type
  { Optional callback that can be used to consume a path data. }
  TBLPathSinkEvent = function(const APath: IBLPath; const AInfo: Pointer): TBLResultCode of object;

type
  { Implements IBLPath }
  TBLPath = class(TInterfacedObject, IBLPath)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPathCore;
  protected
    { IBLPath }
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
    function GetCapacity: Integer;
    function GetVertexData: PBLPoint;
    function GetVertexDataEnd: PBLPoint;
    function GetLastVertex: TBLPoint;
    function GetCommandData: PBLPathCmd;
    function GetCommandDataEnd: PBLPathCmd;
    function GetView: TBLPathView;
    function GetInfoFlags: TBLPathFlags;
    function GetControlBox: TBLBox;
    function GetBoundingBox: TBLBox;
    function GetFigureRange(const AIndex: Integer): TBLRange;
    function GetHandle: PBLPathCore;

    procedure Reset;
    function Clone: IBLPath;
    function Equals(const AOther: IBLPath): Boolean; reintroduce; overload;

    procedure Clear;
    procedure Shrink;
    procedure Reserve(const ACount: Integer);

    procedure SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
      const APt: TBLPoint; const APreserve: Boolean = False); overload;
    procedure SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
      const AX, AY: Double; const APreserve: Boolean = False); overload;

    procedure MoveTo(const AP0: TBLPoint); overload;
    procedure MoveTo(const AX0, AY0: Double); overload;
    procedure LineTo(const AP1: TBLPoint); overload;
    procedure LineTo(const AX1, AY1: Double); overload;
    procedure PolyTo(const APoly: TArray<TBLPoint>); overload;
    procedure PolyTo(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure QuadTo(const AP1, AP2: TBLPoint); overload;
    procedure QuadTo(const AX1, AY1, AX2, AY2: Double); overload;
    procedure CubicTo(const AP1, AP2, AP3: TBLPoint); overload;
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double); overload;
    procedure SmoothQuadTo(const AP2: TBLPoint); overload;
    procedure SmoothQuadTo(const AX2, AY2: Double); overload;
    procedure SmoothCubicTo(const AP2, AP3: TBLPoint); overload;
    procedure SmoothCubicTo(const AX2, AY2, AX3, AY3: Double); overload;
    procedure ArcTo(const AC, AR: TBLPoint; const AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload;
    procedure ArcTo(const ACX, ACY, ARX, ARY, AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload;
    procedure ArcQuadrantTo(const AP1, AP2: TBLPoint); overload;
    procedure ArcQuadrantTo(const AX1, AY1, AX2, AY2: Double); overload;
    procedure EllipticArcTo(const ARP: TBLPoint; const AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AP1: TBLPoint); overload;
    procedure EllipticArcTo(const ARX, ARY, AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AX1, AY1: Double); overload;

    procedure Close;

    procedure AddBox(const ABox: TBLBoxI;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBox(const ABox: TBLBox;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBox(const AX0, AY0, AX1, AY1: Double;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRect(const ARect: TBLRectI;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRect(const ARect: TBLRect;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRect(const AX, AY, AW, AH: Double;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddGeometry(const AGeometryType: TBLGeometryType;
      const AGeometryData: Pointer; const AMatrix: PBLMatrix2D = nil;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW);

    procedure AddCircle(const ACircle: TBLCircle;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddCircle(const ACircle: TBLCircle; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddEllipse(const AEllipse: TBLEllipse;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddEllipse(const AEllipse: TBLEllipse; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRoundRect(const ARoundRect: TBLRoundRect;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRoundRect(const ARoundRect: TBLRoundRect; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddArc(const AArc: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddArc(const AArc: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddChord(const AChord: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddChord(const AChord: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPie(const APie: TBLArc;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPie(const APie: TBLArc; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddLine(const ALine: TBLLine;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddLine(const ALine: TBLLine; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddTriangle(const ATriangle: TBLTriangle;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddTriangle(const ATriangle: TBLTriangle; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRectArray(const ARects: TBLArrayView<TBLRectI>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: TBLArrayView<TBLRectI>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRectI; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRectI; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRectArray(const ARects: TBLArrayView<TBLRect>;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: TBLArrayView<TBLRect>;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRect; const ACount: Integer;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRectArray(const ARects: PBLRect; const ACount: Integer;
      const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddRegion(const ARegion: IBLRegion;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;
    procedure AddRegion(const ARegion: IBLRegion; const AMatrix: TBLMatrix2D;
      const ADirection: TBLGeometryDirection = TBLGeometryDirection.CW); overload;

    procedure AddPath(const APath: IBLPath); overload;
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange); overload;
    procedure AddPath(const APath: IBLPath; const ATranslate: TBLPoint); overload;
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange;
      const ATranslate: TBLPoint); overload;
    procedure AddPath(const APath: IBLPath; const AMatrix: TBLMatrix2D); overload;
    procedure AddPath(const APath: IBLPath; const ARange: TBLRange;
      const AMatrix: TBLMatrix2D); overload;

    procedure AddReversedPath(const APath: IBLPath;
      const AReverseMode: TBLPathReverseMode); overload;
    procedure AddReversedPath(const APath: IBLPath; const ARange: TBLRange;
      const AReverseMode: TBLPathReverseMode); overload;

    procedure AddStrokedPath(const APath: IBLPath;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload;
    procedure AddStrokedPath(const APath: IBLPath; const ARange: TBLRange;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload;

    procedure RemoveRange(const ARange: TBLRange);

    procedure Translate(const AP: TBLPoint); overload;
    procedure Translate(const ARange: TBLRange; const AP: TBLPoint); overload;
    procedure Transform(const AMatrix: TBLMatrix2D); overload;
    procedure Transform(const ARange: TBLRange; const AMatrix: TBLMatrix2D); overload;

    procedure FitTo(const ARect: TBLRect; const AFitFlags: TBLFitFlags = []); overload;
    procedure FitTo(const ARange: TBLRange; const ARect: TBLRect;
      const AFitFlags: TBLFitFlags = []); overload;

    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double): Integer; overload;
    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double;
      out AActualDistance: Double): Integer; overload;

    function HitTest(const AP: TBLPoint; const AFillRule: TBLFillRule): TBLHitTest;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{$ENDREGION 'Path'}

{$REGION 'Format'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Pixel format.

    Compatibility Table
    -------------------

    +---------------------+---------------------+-----------------------------+
    | Blend2D Format      | Cairo Format        | QImage::Format              |
    +---------------------+---------------------+-----------------------------+
    | BL_FORMAT_PRGB32    | CAIRO_FORMAT_ARGB32 | Format_ARGB32_Premultiplied |
    | BL_FORMAT_XRGB32    | CAIRO_FORMAT_RGB24  | Format_RGB32                |
    | BL_FORMAT_A8        | CAIRO_FORMAT_A8     | n/a                         |
    +---------------------+---------------------+-----------------------------+ }
  TBLFormat = (
    { None or invalid pixel format. }
    None   = BL_FORMAT_NONE,

    { 32-bit premultiplied ARGB pixel format (8-bit components). }
    PRGB32 = BL_FORMAT_PRGB32,

    { 32-bit (X)RGB pixel format (8-bit components, alpha ignored). }
    XRGB32 = BL_FORMAT_XRGB32,

    { 8-bit alpha-only pixel format. }
    A8     = BL_FORMAT_A8);

type
  { Pixel format flags. }
  TBLFormatFlag = (
    { Pixel format provides RGB components. }
    RGB           = 0,

    { Pixel format provides only alpha component. }
    Alpha         = 1,

    { Pixel format provides LUM component (and not RGB components). }
    LUM           = 2,

    { Indexed pixel format the requres a palette (I/O only). }
    Indexed       = 4,

    { RGB components are premultiplied by alpha component. }
    Premultiplied = 8,

    { Pixel format doesn't use native byte-order (I/O only). }
    ByteSwap      = 9,

    { The following flags are only informative. They are part of BLFormatInfo,
      but doesn't have to be passed to IBLPixelConverter as they can be easily
      calculated. }

    { Pixel components are byte aligned (all 8bpp). }
    ByteAligned   = 16,

    { Pixel has some undefined bits that represent no information.

      For example a 32-bit XRGB pixel has 8 undefined bits that are usually set
      to all ones so the format can be interpreted as premultiplied RGB as well.
      There are other formats like 16_0555 where the bit has no information and
      is usually set to zero. Blend2D doesn't rely on the content of such bits. }
    UndefinedBits = 17,

    { Little-endian format.
      Note: This is not a real flag that you can test, it's only provided for
      convenience to define little endian pixel formats. }
    LittleEndian  = 0,

    { Big-endian format.
      Note: This is not a real flag that you can test, it's only provided for
      convenience to define little endian pixel formats. }
    BigEndian = ByteSwap);
  TBLFormatFlags = set of TBLFormatFlag;

type
  _TBLFormatFlagsHelper = record helper for TBLFormatFlags
  public const
    { Pixel format provides RGB and Alpha components. }
    RGBA = [TBLFormatFlag.RGB, TBLFormatFlag.Alpha];

    { Pixel format provides LUM and Alpha components (and not RGB components). }
    LUMA = [TBLFormatFlag.LUM, TBLFormatFlag.Alpha];
  end;

{ ============================================================================
   [BLFormatInfo]
  ============================================================================ }

type
  { Provides a detailed information about a pixel format. Use the TBLFormat.Info
    property to get information of Blend2D native pixel formats. }
  TBLFormatInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFormatInfo;
    function GetFlags: TBLFormatFlags; inline;
    procedure SetFlags(const AValue: TBLFormatFlags); inline;
    function GetPalette: PBLRgba32; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLFormatInfo): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLFormatInfo): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const ADepth: Integer; const AFlags: TBLFormatFlags;
      const ARSize, AGSize, ABSize, AASize, ARShift, AGShift, ABShift,
      AAShift: Byte); overload; inline;
    procedure SetSizes(const ARSize, AGSize, ABSize, AASize: Byte); inline;
    procedure SetShifts(const ARShift, AGShift, ABShift, AAShift: Byte); inline;

    { Query Blend2D AFormat and copy it to this format info.

      Raises TBLResultCode.InvalidValue if the format is invalid (and resets
      this record in that case).

      Note: TBLFormat.None is considered an invalid format, thus if it's passed
      to Query, a TBLResultCode.InvalidValue error is raised. }
    procedure Query(const AFormat: TBLFormat);

    { Sanitize this record.

      Sanitizer verifies whether the format is valid and updates the format
      information about flags to values that Blend2D expects. For example
      format flags are properly examined and simplified if possible, byte-swap
      is implicitly performed for formats where a single component matches one
      byte, etc... }
    procedure Sanitize;

    property Depth: Integer read FHandle.depth write FHandle.depth;
    property Flags: TBLFormatFlags read GetFlags write SetFlags;
    property RedSize: Byte read FHandle.rSize write FHandle.rSize;
    property GreenSize: Byte read FHandle.gSize write FHandle.gSize;
    property BlueSize: Byte read FHandle.bSize write FHandle.bSize;
    property AlphaSize: Byte read FHandle.aSize write FHandle.aSize;
    property RedShift: Byte read FHandle.rShift write FHandle.rShift;
    property GreenShift: Byte read FHandle.gShift write FHandle.gShift;
    property BlueShift: Byte read FHandle.bShift write FHandle.bShift;
    property AlphaShift: Byte read FHandle.aShift write FHandle.aShift;
    property Palette: PBLRgba32 read GetPalette;
  end;
  PBLFormatInfo = ^TBLFormatInfo;

type
  { Additional information about pixel formats }
  _TBLFormatHelper = record helper for TBLFormat
  {$REGION 'Internal Declarations'}
  private
    function GetInfo: TBLFormatInfo;
  {$ENDREGION 'Internal Declarations'}
  public
    { Information about this pixel format }
    property Info: TBLFormatInfo read GetInfo;
  end;

{$ENDREGION 'Format'}

{$REGION 'Image'}

{ ============================================================================
   [BLImage - Enums]
  ============================================================================ }
type
  { Flags used by TBLImageInfo. }
  TBLImageInfoFlag = (
    { Progressive mode. }
    Progressive = 0);
  TBLImageInfoFlags = set of TBLImageInfoFlag;

{ ============================================================================
   [BLImage - Info]
  ============================================================================ }

type
  { Image information provided by image codecs. }
  TBLImageInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageInfo;
    function GetCompression: String; inline;
    function GetDensity: TBLSize; inline;
    function GetDepth: Integer; inline;
    function GetFlags: TBLFormatFlags; inline;
    function GetFormat: String; inline;
    function GetFrameCount: Integer; inline;
    function GetPlaneCount: Integer; inline;
    function GetSize: TBLSizeI; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Image size. }
    property Size: TBLSizeI read GetSize;

    { Pixel density per one meter, can contain fractions. }
    property Density: TBLSize read GetDensity;

    { Image flags. }
    property Flags: TBLFormatFlags read GetFlags;

    { Image depth. }
    property Depth: Integer read GetDepth;

    { Number of planes. }
    property PlaneCount: Integer read GetPlaneCount;

    { Number of frames (0 = unknown/unspecified). }
    property FrameCount: Integer read GetFrameCount;

    { Image format (as understood by codec). }
    property Format: String read GetFormat;

    { Image compression (as understood by codec). }
    property Compression: String read GetCompression;
  end;
  PBLImageInfo = ^TBLImageInfo;

{ ============================================================================
   [BLImage - Enums]
  ============================================================================ }

type
  { Filter type used by IBLImage.Scale. }
  TBLImageScaleFilter = (
    { No filter or uninitialized. }
    None     = BL_IMAGE_SCALE_FILTER_NONE,

    { Nearest neighbor filter (radius 1.0). }
    Nearest  = BL_IMAGE_SCALE_FILTER_NEAREST,

    { Bilinear filter (radius 1.0). }
    Bilinear = BL_IMAGE_SCALE_FILTER_BILINEAR,

    { Bicubic filter (radius 2.0). }
    Bicubic  = BL_IMAGE_SCALE_FILTER_BICUBIC,

    { Bell filter (radius 1.5). }
    Bell     = BL_IMAGE_SCALE_FILTER_BELL,

    { Gauss filter (radius 2.0). }
    Gauss    = BL_IMAGE_SCALE_FILTER_GAUSS,

    { Hermite filter (radius 1.0). }
    Hermite  = BL_IMAGE_SCALE_FILTER_HERMITE,

    { Hanning filter (radius 1.0). }
    Hanning  = BL_IMAGE_SCALE_FILTER_HANNING,

    { Catrom filter (radius 2.0). }
    Catrom   = BL_IMAGE_SCALE_FILTER_CATROM,

    { Bessel filter (radius 3.2383). }
    Bessel   = BL_IMAGE_SCALE_FILTER_BESSEL,

    { Sinc filter (radius 2.0, adjustable through TLImageScaleOptions). }
    Sinc     = BL_IMAGE_SCALE_FILTER_SINC,

    { Lanczos filter (radius 2.0, adjustable through TBLImageScaleOptions). }
    Lanczos  = BL_IMAGE_SCALE_FILTER_LANCZOS,

    { Blackman filter (radius 2.0, adjustable through TBLImageScaleOptions). }
    Blackman = BL_IMAGE_SCALE_FILTER_BLACKMAN,

    { Mitchell filter (radius 2.0, parameters 'b' and 'c' passed through
      TBLImageScaleOptions). }
    Mitchell = BL_IMAGE_SCALE_FILTER_MITCHELL,

    { Filter using a user-function, must be passed through
      TBLImageScaleOptions. }
    User     = BL_IMAGE_SCALE_FILTER_USER);

{ ============================================================================
   [BLImage - Typedefs]
  ============================================================================ }

type
  { A user function that can be used by IBLImage.Scale. }
  TBLImageScaleUserFunc = function(const ADst, AArray: PDouble;
    const ACount: NativeInt; const AData: Pointer): Integer; cdecl;

{ ============================================================================
   [BLImage - Data]
  ============================================================================ }

type
  { Data that describes a raster image. Used by IBLImage. }
  TBLImageData = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageData;
    function GetFlags: TBLFormatFlags; inline;
    function GetFormat: TBLFormat; inline;
    function GetSize: TBLSizeI; inline;
    procedure SetFlags(const AValue: TBLFormatFlags); inline;
    procedure SetFormat(const AValue: TBLFormat); inline;
    procedure SetSize(const AValue: TBLSizeI); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property PixelData: Pointer read FHandle.pixelData write FHandle.pixelData;
    property Stride: NativeInt read FHandle.stride write FHandle.stride;
    property Size: TBLSizeI read GetSize write SetSize;
    property Format: TBLFormat read GetFormat write SetFormat;
    property Flags: TBLFormatFlags read GetFlags write SetFlags;
  end;
  PBLImageData = ^TBLImageData;

{ ============================================================================
   [BLImage - ScaleOptions]
  ============================================================================ }

type
  { Options that can used to customize image scaling. }
  TBLImageScaleOptions = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageScaleOptions;
    function GetUserFunc: TBLImageScaleUserFunc; inline;
    procedure SetUserFunc(const AValue: TBLImageScaleUserFunc); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;
    procedure ResetToDefaults; inline;

    property UserFunc: TBLImageScaleUserFunc read GetUserFunc write SetUserFunc;
    property UserData: Pointer read FHandle.userData write FHandle.userData;
    property Radius: Double read FHandle.radius write FHandle.radius;
    property MitchellB: Double read FHandle.mitchell.b write FHandle.mitchell.b;
    property MitchellC: Double read FHandle.mitchell.c write FHandle.mitchell.c;
  end;
  PBLImageScaleOptions = ^TBLImageScaleOptions;

type
  IBLImage = interface;
  IBLImageCodec = interface;

  { This event is called when IBLImage.InitializeFromData is used and the image
    is destroyed. }
  TBLImageDestroyEvent = procedure (const AImage: IBLImage) of object;

  { 2D raster image }
  IBLImage = interface
  ['{9E756ED1-76AB-4318-92BD-F6D7F14447A5}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetSize: TBLSizeI;
    function GetFormat: TBLFormat;
    function GetHandle: PBLImageCore;
    {$ENDREGION 'Internal Declarations'}

    { Initializes a new image of a specified width, height, and format. }
    procedure Initialize(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat = TBLFormat.PRGB32);

    { Initializes a new image from external data.
      The pixel data is *not* copied and must stay alive as long as the image
      is alive. Use AOnDestroy to get notified when the image is destroyed and
      you can safely free the pixel data. }
    procedure InitializeFromData(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat; const APixelData: Pointer;
      const AStride: Integer; const AOnDestroy: TBLImageDestroyEvent = nil);

    procedure Reset;
    function Clone: IBLImage;
    function Equals(const AOther: IBLImage): Boolean;
    procedure GetData(out AData: TBLImageData);
    procedure MakeMutable; overload;
    procedure MakeMutable(out AData: TBLImageData); overload;
    procedure Convert(const AFormat: TBLFormat);
    procedure ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter); overload;
    procedure ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter; const AOptions: TBLImageScaleOptions); overload;

    procedure ReadFromFile(const AFilename: String); overload;
    procedure ReadFromFile(const AFilename: String;
      const ACodecs: TArray<IBLImageCodec>); overload;

    procedure ReadFromData(const AData: Pointer; const ASize: Integer); overload;
    procedure ReadFromData(const AData: Pointer; const ASize: Integer;
      const ACodecs: TArray<IBLImageCodec>); overload;
    procedure ReadFromData(const AData: TBytes); overload;
    procedure ReadFromData(const AData: TBytes;
      const ACodecs: TArray<IBLImageCodec>); overload;
    procedure ReadFromData(const AView: TBLArrayView<Byte>); overload;
    procedure ReadFromData(const AView: TBLArrayView<Byte>;
      const ACodecs: TArray<IBLImageCodec>); overload;

    procedure WriteToFile(const AFilename: String); overload;
    procedure WriteToFile(const AFilename: String; const ACodec: IBLImageCodec); overload;
    function WriteToData(const ACodec: IBLImageCodec): TBytes;

    { Tests whether the image is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the image is empty (has no size). }
    property IsEmpty: Boolean read GetIsEmpty;

    { Image width. }
    property Width: Integer read GetWidth;

    { Image height. }
    property Height: Integer read GetHeight;

    { Image size. }
    property Size: TBLSizeI read GetSize;

    { Image format. }
    property Format: TBLFormat read GetFormat;

    { Internal handle for use with the C API }
    property Handle: PBLImageCore read GetHandle;
  end;

  { Implements IBLImage }
  TBLImage = class(TInterfacedObject, IBLImage)
  {$REGION 'Internal Declarations'}
  private type
    TDestroyData = record
      Image: IBLImage;
      Event: TBLImageDestroyEvent;
    end;
    PDestroyData = ^TDestroyData;
  private
    FHandle: BLImageCore;
    FIsReference: Boolean;
  private
    class procedure DoDestroy(impl, destroyData: Pointer); cdecl; static;
  protected
    { IBLImage }
    function GetIsNone: Boolean;
    function GetIsEmpty: Boolean;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetSize: TBLSizeI;
    function GetFormat: TBLFormat;
    function GetHandle: PBLImageCore;

    procedure Initialize(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat);

    procedure InitializeFromData(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat; const APixelData: Pointer;
      const AStride: Integer; const AOnDestroy: TBLImageDestroyEvent);

    procedure Reset;
    function Clone: IBLImage;
    function Equals(const AOther: IBLImage): Boolean; reintroduce; overload;
    procedure GetData(out AData: TBLImageData);
    procedure MakeMutable; overload;
    procedure MakeMutable(out AData: TBLImageData); overload;
    procedure Convert(const AFormat: TBLFormat);
    procedure ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter); overload;
    procedure ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter; const AOptions: TBLImageScaleOptions); overload;

    procedure ReadFromFile(const AFilename: String); overload;
    procedure ReadFromFile(const AFilename: String;
      const ACodecs: TArray<IBLImageCodec>); overload;

    procedure ReadFromData(const AData: Pointer; const ASize: Integer); overload;
    procedure ReadFromData(const AData: Pointer; const ASize: Integer;
      const ACodecs: TArray<IBLImageCodec>); overload;
    procedure ReadFromData(const AData: TBytes); overload;
    procedure ReadFromData(const AData: TBytes;
      const ACodecs: TArray<IBLImageCodec>); overload;
    procedure ReadFromData(const AView: TBLArrayView<Byte>); overload;
    procedure ReadFromData(const AView: TBLArrayView<Byte>;
      const ACodecs: TArray<IBLImageCodec>); overload;

    procedure WriteToFile(const AFilename: String); overload;
    procedure WriteToFile(const AFilename: String; const ACodec: IBLImageCodec); overload;
    function WriteToData(const ACodec: IBLImageCodec): TBytes;
  private
    constructor Create(const AHandle: BLImageCore; const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    constructor Create(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat = TBLFormat.PRGB32); overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
    class procedure Scale(const ASrc, ADst: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter); overload; static;
    class procedure Scale(const ASrc, ADst: IBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter; const AOptions: TBLImageScaleOptions); overload; static;
  end;

{$ENDREGION 'Image'}

{$REGION 'Image Codec'}

{ ============================================================================
   [BLImageCodec - Enums]
  ============================================================================ }

  { Image codec feature bits. }
  TBLImageCodecFeature = (
    { Image codec supports reading images (can create IBLImageDecoder). }
    Read       = 0,

    { Image codec supports writing images (can create IBLImageEncoder). }
    Write      = 1,

    { Image codec supports lossless compression. }
    Lossless   = 2,

    { Image codec supports lossy compression. }
    Lossy      = 3,

    { Image codec supports writing multiple frames (GIF). }
    MultiFrame = 4,

    { Image codec supports IPTC metadata. }
    IPTC       = 28,

    { Image codec supports EXIF metadata. }
    EXIF       = 29,

    { Image codec supports XMP metadata. }
    XMP        = 30);
  TBLImageCodecFeatures = set of TBLImageCodecFeature;

{ ============================================================================
   [BLImageDecoder]
  ============================================================================ }

  { Image decoder }
  IBLImageDecoder = interface
  ['{D68437C2-E6EF-46F1-9191-28F844EF17C6}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetLastResult: TBLResultCode;
    function GetFrameIndex: Integer;
    function GetBufferIndex: NativeInt;
    function GetHandle: PBLImageDecoderCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Equals(const AOther: IBLImageDecoder): Boolean;

    procedure Restart;

    procedure ReadInfo(const ABuffer: TBytes; out AInfo: TBLImageInfo); overload;
    procedure ReadInfo(const ABuffer: TBLArrayView<Byte>; out AInfo: TBLImageInfo); overload;
    procedure ReadInfo(const ABuffer: Pointer; const ASize: Integer; out AInfo: TBLImageInfo); overload;

    function ReadFrame(const ABuffer: TBytes): IBLImage; overload;
    function ReadFrame(const ABuffer: TBLArrayView<Byte>): IBLImage; overload;
    function ReadFrame(const ABuffer: Pointer; const ASize: Integer): IBLImage; overload;

    { Tests whether the image decoder is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Last decoding result. }
    property LastResult: TBLResultCode read GetLastResult;

    { Current frame index (to be decoded). }
    property FrameIndex: Integer read GetFrameIndex;

    { Position in source buffer. }
    property BufferIndex: NativeInt read GetBufferIndex;

    { Internal handle for use with the C API }
    property Handle: PBLImageDecoderCore read GetHandle;
  end;

  { Implements IBLImageDecoder }
  TBLImageDecoder = class(TInterfacedObject, IBLImageDecoder)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageDecoderCore;
  protected
    { IBLImageDecoder }
    function GetIsNone: Boolean;
    function GetLastResult: TBLResultCode;
    function GetFrameIndex: Integer;
    function GetBufferIndex: NativeInt;
    function GetHandle: PBLImageDecoderCore;

    procedure Reset;
    function Equals(const AOther: IBLImageDecoder): Boolean; reintroduce; overload;

    procedure Restart;

    procedure ReadInfo(const ABuffer: TBytes; out AInfo: TBLImageInfo); overload;
    procedure ReadInfo(const ABuffer: TBLArrayView<Byte>; out AInfo: TBLImageInfo); overload;
    procedure ReadInfo(const ABuffer: Pointer; const ASize: Integer; out AInfo: TBLImageInfo); overload;

    function ReadFrame(const ABuffer: TBytes): IBLImage; overload;
    function ReadFrame(const ABuffer: TBLArrayView<Byte>): IBLImage; overload;
    function ReadFrame(const ABuffer: Pointer; const ASize: Integer): IBLImage; overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{ ============================================================================
   [BLImageEncoder]
  ============================================================================ }

  { Image encoder }
  IBLImageEncoder = interface
  ['{D754FC7A-AC83-4543-8200-AE896920FFB0}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetLastResult: TBLResultCode;
    function GetFrameIndex: Integer;
    function GetBufferIndex: NativeInt;
    function GetHandle: PBLImageEncoderCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Equals(const AOther: IBLImageEncoder): Boolean;

    procedure Restart;

    { Encodes the given AImage and returns the encoded data. }
    function WriteFrame(const AImage: IBLImage): TBytes;

    { Tests whether the image encoder is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Last encoding result. }
    property LastResult: TBLResultCode read GetLastResult;

    { Current frame index (yet to be written). }
    property FrameIndex: Integer read GetFrameIndex;

    { Position in destination buffer. }
    property BufferIndex: NativeInt read GetBufferIndex;

    { Internal handle for use with the C API }
    property Handle: PBLImageEncoderCore read GetHandle;
  end;

  { Implements IBLImageEncoder }
  TBLImageEncoder = class(TInterfacedObject, IBLImageEncoder)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageEncoderCore;
  protected
    { IBLImageEncoder }
    function GetIsNone: Boolean;
    function GetLastResult: TBLResultCode;
    function GetFrameIndex: Integer;
    function GetBufferIndex: NativeInt;
    function GetHandle: PBLImageEncoderCore;

    procedure Reset;
    function Equals(const AOther: IBLImageEncoder): Boolean; reintroduce; overload;

    procedure Restart;

    function WriteFrame(const AImage: IBLImage): TBytes;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{ ============================================================================
   [BLImageCodec]
  ============================================================================ }

  { Image codec.
    Provides a unified interface for inspecting image data and creating image
    decoders & encoders. }
  IBLImageCodec = interface
  ['{5F2E0C15-3CF5-4454-ADA4-CEE5D4A3B542}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetName: String;
    function GetVendor: String;
    function GetMimeType: String;
    function GetExtensions: String;
    function GetFeatures: TBLImageCodecFeatures;
    function GetHandle: PBLImageCodecCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Equals(const AOther: IBLImageCodec): Boolean;

    { Tests whether the image codec has a given feature. }
    function HasFeature(const AFeature: TBLImageCodecFeature): Boolean;

    function FindByName(const AName: String): Boolean; overload;
    function FindByName(const AName: String; const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByExtension(const AExt: String): Boolean; overload;
    function FindByExtension(const AExt: String; const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByData(const AData: Pointer; const ASize: Integer): Boolean; overload;
    function FindByData(const AData: Pointer; const ASize: Integer;
      const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByData(const AData: TBytes): Boolean; overload;
    function FindByData(const AData: TBytes;
      const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    { Returns a score }
    function InspectData(const ABuffer: TBytes): Cardinal; overload;
    function InspectData(const ABuffer: TBLArrayView<Byte>): Cardinal; overload;
    function InspectData(const ABuffer: Pointer; const ASize: Integer): Cardinal; overload;

    { Tests whether the image codec is a built-in nil instance. }
    property IsNone: Boolean read GetIsNone;

    { Image codec name (i.e, "PNG", "JPEG", etc...). }
    property Name: String read GetName;

    { The image codec vendor (i.e. "Blend2D" for all built-in codecs). }
    property Vendor: String read GetVendor;

    { Mime-type associated with the image codec's format. }
    property MimeType: String read GetMimeType;

    { A list of file extensions used to store image of this codec, separated by
      '|' character. }
    property Extensions: String read GetExtensions;

    { Image codec features }
    property Features: TBLImageCodecFeatures read GetFeatures;

    { Internal handle for use with the C API }
    property Handle: PBLImageCodecCore read GetHandle;
  end;

type
  { Implements IBLImageCodec }
  TBLImageCodec = class(TInterfacedObject, IBLImageCodec)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLImageCodecCore;
    FIsReference: Boolean;
  private
    class function GetBuiltInCodecs: TArray<IBLImageCodec>; static;
  protected
    { IBLImageCodec }
    function GetIsNone: Boolean;
    function GetName: String;
    function GetVendor: String;
    function GetMimeType: String;
    function GetExtensions: String;
    function GetFeatures: TBLImageCodecFeatures;
    function GetHandle: PBLImageCodecCore;

    procedure Reset;
    function Equals(const AOther: IBLImageCodec): Boolean; reintroduce; overload;
    function HasFeature(const AFeature: TBLImageCodecFeature): Boolean;

    function FindByName(const AName: String): Boolean; overload;
    function FindByName(const AName: String; const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByExtension(const AExt: String): Boolean; overload;
    function FindByExtension(const AExt: String; const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByData(const AData: Pointer; const ASize: Integer): Boolean; overload;
    function FindByData(const AData: Pointer; const ASize: Integer;
      const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function FindByData(const AData: TBytes): Boolean; overload;
    function FindByData(const AData: TBytes;
      const ACodecs: TArray<IBLImageCodec>): Boolean; overload;

    function InspectData(const ABuffer: TBytes): Cardinal; overload;
    function InspectData(const ABuffer: TBLArrayView<Byte>): Cardinal; overload;
    function InspectData(const ABuffer: Pointer; const ASize: Integer): Cardinal; overload;
  protected
    constructor Create(const AHandle: BLImageCodecCore; const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
    class procedure AddToBuiltIn(const ACodec: IBLImageCodec); static;
    class procedure RemoveFromBuiltIn(const ACodec: IBLImageCodec); static;

    class property BuiltInCodecs: TArray<IBLImageCodec> read GetBuiltInCodecs;
  end;

{$ENDREGION 'Image Codec'}

{$REGION 'Pattern'}

{ ============================================================================
   [BLPattern]
  ============================================================================ }

type
  { Pattern }
  IBLPattern = interface
  ['{BE559BED-3495-49D7-AB00-7BF092E1F53B}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetImage: IBLImage;
    procedure SetImage(const AValue: IBLImage); overload;
    function GetArea: TBLRectI;
    procedure SetArea(const AValue: TBLRectI);
    function GetExtendMode: TBLExtendMode;
    procedure SetExtendMode(const AValue: TBLExtendMode);
    function GetHasMatrix: Boolean;
    function GetMatrixType: TBLMatrix2DType;
    function GetMatrix: TBLMatrix2D;
    procedure SetMatrix(const AValue: TBLMatrix2D);
    function GetHandle: PBLPatternCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Initialize(const AImage: IBLImage;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    procedure Initialize(const AImage: IBLImage; const AExtendMode: TBLExtendMode;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    procedure Initialize(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D); overload;

    procedure Reset;
    function Equals(const AOther: IBLPattern): Boolean;

    procedure SetImage(const AValue: IBLImage; const AArea: TBLRectI); overload;
    procedure ResetImage;
    procedure ResetArea;
    procedure ResetExtendMode;
    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const AP: TBLPoint); overload;
    procedure Translate(const AP: TBLPointI); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const AP: TBLPoint); overload;
    procedure Scale(const AP: TBLPointI); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const AP: TBLPoint); overload;
    procedure PostTranslate(const AP: TBLPointI); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const AP: TBLPoint); overload;
    procedure PostScale(const AP: TBLPointI); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);

    { Whether the pattern is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    property Image: IBLImage read GetImage write SetImage;
    property Area: TBLRectI read GetArea write SetArea;
    property ExtendMode: TBLExtendMode read GetExtendMode write SetExtendMode;

    property HasMatrix: Boolean read GetHasMatrix;
    property MatrixType: TBLMatrix2DType read GetMatrixType;
    property Matrix: TBLMatrix2D read GetMatrix write SetMatrix;

    { Internal handle for use with the C API }
    property Handle: PBLPatternCore read GetHandle;
  end;

type
  { Implements IBLPattern }
  TBLPattern = class(TInterfacedObject, IBLPattern)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPatternCore;
    FImage: IBLImage;
    FIsReference: Boolean;
  protected
    { IBLPattern }
    function GetIsNone: Boolean;
    function GetImage: IBLImage;
    procedure SetImage(const AValue: IBLImage); overload;
    function GetArea: TBLRectI;
    procedure SetArea(const AValue: TBLRectI);
    function GetExtendMode: TBLExtendMode;
    procedure SetExtendMode(const AValue: TBLExtendMode);
    function GetHasMatrix: Boolean;
    function GetMatrixType: TBLMatrix2DType;
    function GetMatrix: TBLMatrix2D;
    procedure SetMatrix(const AValue: TBLMatrix2D);
    function GetHandle: PBLPatternCore;

    procedure Initialize(const AImage: IBLImage;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    procedure Initialize(const AImage: IBLImage; const AExtendMode: TBLExtendMode;
      const AMatrix: TBLMatrix2D); overload;
    procedure Initialize(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    procedure Initialize(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D); overload;

    procedure Reset;
    function Equals(const AOther: IBLPattern): Boolean; reintroduce; overload;

    procedure SetImage(const AValue: IBLImage; const AArea: TBLRectI); overload;
    procedure ResetImage;
    procedure ResetArea;
    procedure ResetExtendMode;
    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const AP: TBLPoint); overload;
    procedure Translate(const AP: TBLPointI); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const AP: TBLPoint); overload;
    procedure Scale(const AP: TBLPointI); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure Rotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const AP: TBLPoint); overload;
    procedure PostTranslate(const AP: TBLPointI); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const AP: TBLPoint); overload;
    procedure PostScale(const AP: TBLPointI); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double; const AP: TBLPointI); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);
  private
    constructor Create(const AHandle: BLPatternCore;
      const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    constructor Create(const AImage: IBLImage;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    constructor Create(const AImage: IBLImage; const AExtendMode: TBLExtendMode;
      const AMatrix: TBLMatrix2D); overload;
    constructor Create(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode = TBLExtendMode.&Repeat); overload;
    constructor Create(const AImage: IBLImage; const AArea: TBLRectI;
      const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D); overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{$ENDREGION 'Pattern'}

{$REGION 'Font Defs'}

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Placement of glyphs stored in a TBLGlyphRun. }
  TBLGlyphPlacementType = (
    { No placement (custom handling by TBLPathSinkEvent). }
    None          = BL_GLYPH_PLACEMENT_TYPE_NONE,

    { Each glyph has a TBLGlyphPlacement (advance + offset). }
    AdvanceOffset = BL_GLYPH_PLACEMENT_TYPE_ADVANCE_OFFSET,

    { Each glyph has a TBLPoint offset in design-space units. }
    DesignUnits   = BL_GLYPH_PLACEMENT_TYPE_DESIGN_UNITS,

    { Each glyph has a TBLPoint offset in user-space units. }
    UserUnits     = BL_GLYPH_PLACEMENT_TYPE_USER_UNITS,

    { Each glyph has a TBLPoint offset in absolute units. }
    AbsoluteUnits = BL_GLYPH_PLACEMENT_TYPE_ABSOLUTE_UNITS);

type
  TBLGlyphRunFlag = (
    _Dummy          = 0,

    { Glyph-run contains USC-4 string and not glyphs (glyph-buffer only). }
    UCS4Content     = 28,

    { Glyph-run was created from text that was not a valid unicode. }
    InvalidText     = 29,

    { Not the whole text was mapped to glyphs (contains undefined glyphs). }
    UndefinedGlyphs = 30,

    { Encountered invalid font-data during text / glyph processing. }
    InvalidFontData = 31);

  TBLGlyphRunFlags = set of TBLGlyphRunFlag;

type
  { Font-data flags. }
  TBLFontDataFlag = (
    { Font data references a font-collection. }
    Collection = 0);
  TBLFontDataFlags = set of TBLFontDataFlag;

type
  { Type of a font or font-face. }
  TBLFontFaceType = (
    None     = BL_FONT_FACE_TYPE_NONE,

    { TrueType/OpenType font type. }
    OpenType = BL_FONT_FACE_TYPE_OPENTYPE);

type
  TBLFontFaceFlag = (
    { Font uses typographic family and subfamily names. }
    TypographicNames   = 0,

    { Font uses typographic metrics. }
    TypographicMetrics = 1,

    { Character to glyph mapping is available. }
    CharToGlyphMapping = 2,

    { Horizontal glyph metrics (advances, side bearings) is available. }
    HorizontalMetrics  = 4,

    { Vertical glyph metrics (advances, side bearings) is available. }
    VerticalMetrics    = 5,

    { Legacy horizontal kerning feature ('kern' table with horizontal kerning data). }
    HorizontalKerning  = 6,

    { Legacy vertical kerning feature ('kern' table with vertical kerning data). }
    VerticalKerning    = 7,

    { OpenType features (GDEF, GPOS, GSUB) are available. }
    OpenTypeFeatures   = 8,

    { Panose classification is available. }
    PanoseData         = 9,

    { Unicode coverage information is available. }
    UnicodeCoverage    = 10,

    { Baseline for font at `y` equals 0. }
    BaselineYEquals0   = 12,

    { Left sidebearing point at `x == 0` (TT only). }
    LSBPointXEquals0   = 13,

    { Unicode variation sequences feature is available. }
    VariationSequences = 28,

    { OpenType Font Variations feature is available. }
    OpenTypeVariations = 29,

    { This is a symbol font. }
    SymbolFont         = 30,

    { This is a last resort font. }
    LastResortFont     = 31);
  TBLFontFaceFlags = set of TBLFontFaceFlag;

type
  TBLFontFaceDiagFlag = (
    { Wrong data in 'name' table. }
    WrongNameData   = 0,

    { Fixed data read from 'name' table and possibly fixed font family/subfamily
      name. }
    FixedNameData   = 1,

    { Wrong data in 'kern' table [kerning disabled]. }
    WrongKernData   = 2,

    { Fixed data read from 'kern' table so it can be used. }
    FixedKernData   = 3,

    { Wrong data in 'cmap' table. }
    WrongCMAPData   = 4,

    { Wrong format in 'cmap' (sub)table. }
    WrongCMAPFormat = 5,

    { Wrong data in 'GDEF' table. }
    WrongGDEFData   = 8,

    { Wrong data in 'GPOS' table. }
    WrongGPOSData   = 10,

    { Wrong data in 'GSUB' table. }
    WrongGSUBData   = 12);
  TBLFontFaceDiagFlags = set of TBLFontFaceDiagFlag;

type
  { Format of an outline stored in a font. }
  TBLFontOutlineType = (
    { None. }
    None     = BL_FONT_OUTLINE_TYPE_NONE,

    { TrueType outlines. }
    TrueType = BL_FONT_OUTLINE_TYPE_TRUETYPE,

    { OpenType (CFF) outlines. }
    CFF      = BL_FONT_OUTLINE_TYPE_CFF,

    { OpenType (CFF2) outlines (font variations support). }
    CFF2     = BL_FONT_OUTLINE_TYPE_CFF2);

type
  { Font stretch. }
  TBLFontStretch = (
    { Ultra condensed stretch. }
    UltraCondensed = BL_FONT_STRETCH_ULTRA_CONDENSED,

    { Extra condensed stretch. }
    ExtraCondensed = BL_FONT_STRETCH_EXTRA_CONDENSED,

    { Condensed stretch. }
    Condensed      = BL_FONT_STRETCH_CONDENSED,

    { Semi condensed stretch. }
    SemiCondensed  = BL_FONT_STRETCH_SEMI_CONDENSED,

    { Normal stretch. }
    Normal         = BL_FONT_STRETCH_NORMAL,

    { Semi expanded stretch. }
    SemiExpanded   = BL_FONT_STRETCH_SEMI_EXPANDED,

    { Expanded stretch. }
    Expanded       = BL_FONT_STRETCH_EXPANDED,

    { Extra expanded stretch. }
    ExtraExpanded  = BL_FONT_STRETCH_EXTRA_EXPANDED,

    { Ultra expanded stretch. }
    UltraExpanded  = BL_FONT_STRETCH_ULTRA_EXPANDED);

type
  { Font style. }
  TBLFontStyle = (
    { Normal style. }
    Normal = BL_FONT_STYLE_NORMAL,

    { Oblique. }
    Oblique = BL_FONT_STYLE_OBLIQUE,

    { Italic. }
    Italic  = BL_FONT_STYLE_ITALIC);

type
  { Font weight. }
  TBLFontWeight = (
    { Thin weight (100). }
    Thin = BL_FONT_WEIGHT_THIN,

    { Extra light weight (200). }
    ExtraLight = BL_FONT_WEIGHT_EXTRA_LIGHT,

    { Light weight (300). }
    Light      = BL_FONT_WEIGHT_LIGHT,

    { Semi light weight (350). }
    SemiLight  = BL_FONT_WEIGHT_SEMI_LIGHT,

    { Normal weight (400). }
    Normal     = BL_FONT_WEIGHT_NORMAL,

    { Medium weight (500). }
    Medium     = BL_FONT_WEIGHT_MEDIUM,

    { Semi bold weight (600). }
    SemiBold   = BL_FONT_WEIGHT_SEMI_BOLD,

    { Bold weight (700). }
    Bold       = BL_FONT_WEIGHT_BOLD,

    { Extra bold weight (800). }
    ExtraBold  = BL_FONT_WEIGHT_EXTRA_BOLD,

    { Black weight (900). }
    Black      = BL_FONT_WEIGHT_BLACK,

    { Extra black weight (950). }
    ExtraBlack = BL_FONT_WEIGHT_EXTRA_BLACK);

type
  { Font string identifiers used by OpenType 'name' table. }
  TBLFontStringId = (
    { Copyright notice. }
    CopyrightNotice           = BL_FONT_STRING_COPYRIGHT_NOTICE,

    { Font family name. }
    FamilyName                = BL_FONT_STRING_FAMILY_NAME,

    { Font subfamily name. }
    SubfamilyName             = BL_FONT_STRING_SUBFAMILY_NAME,

    { Unique font identifier. }
    UniqueIdentifier          = BL_FONT_STRING_UNIQUE_IDENTIFIER,

    { Full font name that reflects all family and relevant subfamily descriptors. }
    FullName                  = BL_FONT_STRING_FULL_NAME,

    { Version string. Should begin with the synta `Version <number>.<number>`. }
    VersionString             = BL_FONT_STRING_VERSION_STRING,

    { PostScript name for the font. }
    PostScriptName            = BL_FONT_STRING_POST_SCRIPT_NAME,

    { Trademark notice/information for this font. }
    Trademark                 = BL_FONT_STRING_TRADEMARK,

    { Manufacturer name. }
    ManufacturerName          = BL_FONT_STRING_MANUFACTURER_NAME,

    { Name of the designer of the typeface. }
    DesignerName              = BL_FONT_STRING_DESIGNER_NAME,

    { Description of the typeface. }
    Description               = BL_FONT_STRING_DESCRIPTION,

    { URL of font vendor. }
    VendorURL                 = BL_FONT_STRING_VENDOR_URL,

    { URL of typeface designer. }
    DesignerURL               = BL_FONT_STRING_DESIGNER_URL,

    { Description of how the font may be legally used. }
    LicenseDescription        = BL_FONT_STRING_LICENSE_DESCRIPTION,

    { URL where additional licensing information can be found. }
    LicenseInfoURL            = BL_FONT_STRING_LICENSE_INFO_URL,

    { Reserved. }
    Reserved                  = BL_FONT_STRING_RESERVED,

    { Typographic family name. }
    TypographicFamilyName     = BL_FONT_STRING_TYPOGRAPHIC_FAMILY_NAME,

    { Typographic subfamily name. }
    TypographicSubfamilyName  = BL_FONT_STRING_TYPOGRAPHIC_SUBFAMILY_NAME,

    { Compatible full name (MAC only). }
    CompatibleFullName        = BL_FONT_STRING_COMPATIBLE_FULL_NAME,

    { Sample text - font name or any other text from the designer. }
    SampleText                = BL_FONT_STRING_SAMPLE_TEXT,

    { PostScript CID findfont name. }
    PostScriptCIDName         = BL_FONT_STRING_POST_SCRIPT_CID_NAME,

    { WWS family name. }
    WWSFamilyName             = BL_FONT_STRING_WWS_FAMILY_NAME,

    { WWS subfamily name. }
    WWSSubfamilyName          = BL_FONT_STRING_WWS_SUBFAMILY_NAME,

    { Light background palette. }
    LightBackgroundPalette    = BL_FONT_STRING_LIGHT_BACKGROUND_PALETTE,

    { Dark background palette. }
    DarkBackgroundPalette     = BL_FONT_STRING_DARK_BACKGROUND_PALETTE,

    { Variations PostScript name prefix. }
    VariationsPostSciptPrefix = BL_FONT_STRING_VARIATIONS_POST_SCRIPT_PREFIX);

type
  { Bit positions in TBLFontUnicodeCoverage record.
    Each bit represents a range (or multiple ranges) of unicode characters. }
  TBLFontUnicodeCoverageIndex = (
    BasicLatin                          = BL_FONT_UC_INDEX_BASIC_LATIN,                              // [000000-00007F] Basic Latin.
    Latin1Supplement                    = BL_FONT_UC_INDEX_LATIN1_SUPPLEMENT,                        // [000080-0000FF] Latin-1 Supplement.
    LatinExtendedA                      = BL_FONT_UC_INDEX_LATIN_EXTENDED_A,                         // [000100-00017F] Latin Extended-A.
    LatinExtendedB                      = BL_FONT_UC_INDEX_LATIN_EXTENDED_B,                         // [000180-00024F] Latin Extended-B.
    IPAExtensions                       = BL_FONT_UC_INDEX_IPA_EXTENSIONS,                           // [000250-0002AF] IPA Extensions.
                                                                                                     // [001D00-001D7F] Phonetic Extensions.
                                                                                                     // [001D80-001DBF] Phonetic Extensions Supplement.
    SpacingModifierLetters              = BL_FONT_UC_INDEX_SPACING_MODIFIER_LETTERS,                 // [0002B0-0002FF] Spacing Modifier Letters.
                                                                                                     // [00A700-00A71F] Modifier Tone Letters.
                                                                                                     // [001DC0-001DFF] Combining Diacritical Marks Supplement.
    CombiningDiacriticalMarks           = BL_FONT_UC_INDEX_COMBINING_DIACRITICAL_MARKS,              // [000300-00036F] Combining Diacritical Marks.
    GreekAndCoptic                      = BL_FONT_UC_INDEX_GREEK_AND_COPTIC,                         // [000370-0003FF] Greek and Coptic.
    Coptic                              = BL_FONT_UC_INDEX_COPTIC,                                   // [002C80-002CFF] Coptic.
    Cyrillic                            = BL_FONT_UC_INDEX_CYRILLIC,                                 // [000400-0004FF] Cyrillic.
                                                                                                     // [000500-00052F] Cyrillic Supplement.
                                                                                                     // [002DE0-002DFF] Cyrillic Extended-A.
                                                                                                     // [00A640-00A69F] Cyrillic Extended-B.
    Armenian                            = BL_FONT_UC_INDEX_ARMENIAN,                                 // [000530-00058F] Armenian.
    Hebrew                              = BL_FONT_UC_INDEX_HEBREW,                                   // [000590-0005FF] Hebrew.
    Vai                                 = BL_FONT_UC_INDEX_VAI,                                      // [00A500-00A63F] Vai.
    Arabic                              = BL_FONT_UC_INDEX_ARABIC,                                   // [000600-0006FF] Arabic.
                                                                                                     // [000750-00077F] Arabic Supplement.
    NKo                                 = BL_FONT_UC_INDEX_NKO,                                      // [0007C0-0007FF] NKo.
    Devanagari                          = BL_FONT_UC_INDEX_DEVANAGARI,                               // [000900-00097F] Devanagari.
    Bengali                             = BL_FONT_UC_INDEX_BENGALI,                                  // [000980-0009FF] Bengali.
    Gurmukhi                            = BL_FONT_UC_INDEX_GURMUKHI,                                 // [000A00-000A7F] Gurmukhi.
    Gujarati                            = BL_FONT_UC_INDEX_GUJARATI,                                 // [000A80-000AFF] Gujarati.
    Oriya                               = BL_FONT_UC_INDEX_ORIYA,                                    // [000B00-000B7F] Oriya.
    Tamil                               = BL_FONT_UC_INDEX_TAMIL,                                    // [000B80-000BFF] Tamil.
    Telugu                              = BL_FONT_UC_INDEX_TELUGU,                                   // [000C00-000C7F] Telugu.
    Kannada                             = BL_FONT_UC_INDEX_KANNADA,                                  // [000C80-000CFF] Kannada.
    Malayalam                           = BL_FONT_UC_INDEX_MALAYALAM,                                // [000D00-000D7F] Malayalam.
    Thai                                = BL_FONT_UC_INDEX_THAI,                                     // [000E00-000E7F] Thai.
    Lao                                 = BL_FONT_UC_INDEX_LAO,                                      // [000E80-000EFF] Lao.
    Georgian                            = BL_FONT_UC_INDEX_GEORGIAN,                                 // [0010A0-0010FF] Georgian.
                                                                                                     // [002D00-002D2F] Georgian Supplement.
    Balinese                            = BL_FONT_UC_INDEX_BALINESE,                                 // [001B00-001B7F] Balinese.
    HangulJamo                          = BL_FONT_UC_INDEX_HANGUL_JAMO,                              // [001100-0011FF] Hangul Jamo.
    LatinExtendedAdditional             = BL_FONT_UC_INDEX_LATIN_EXTENDED_ADDITIONAL,                // [001E00-001EFF] Latin Extended Additional.
                                                                                                     // [002C60-002C7F] Latin Extended-C.
                                                                                                     // [00A720-00A7FF] Latin Extended-D.
    GreekExtended                       = BL_FONT_UC_INDEX_GREEK_EXTENDED,                           // [001F00-001FFF] Greek Extended.
    GeneralPunctuation                  = BL_FONT_UC_INDEX_GENERAL_PUNCTUATION,                      // [002000-00206F] General Punctuation.
                                                                                                     // [002E00-002E7F] Supplemental Punctuation.
    SuperscriptsAndSubscripts           = BL_FONT_UC_INDEX_SUPERSCRIPTS_AND_SUBSCRIPTS,              // [002070-00209F] Superscripts And Subscripts.
    CurrencySymbols                     = BL_FONT_UC_INDEX_CURRENCY_SYMBOLS,                         // [0020A0-0020CF] Currency Symbols.
    CombiningDiacriticalMarksForSymbols = BL_FONT_UC_INDEX_COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS,  // [0020D0-0020FF] Combining Diacritical Marks For Symbols.
    LetterlikeSymbols                   = BL_FONT_UC_INDEX_LETTERLIKE_SYMBOLS,                       // [002100-00214F] Letterlike Symbols.
    NumberForms                         = BL_FONT_UC_INDEX_NUMBER_FORMS,                             // [002150-00218F] Number Forms.
    Arrows                              = BL_FONT_UC_INDEX_ARROWS,                                   // [002190-0021FF] Arrows.
                                                                                                     // [0027F0-0027FF] Supplemental Arrows-A.
                                                                                                     // [002900-00297F] Supplemental Arrows-B.
                                                                                                     // [002B00-002BFF] Miscellaneous Symbols and Arrows.
    MathematicalOperators               = BL_FONT_UC_INDEX_MATHEMATICAL_OPERATORS,                   // [002200-0022FF] Mathematical Operators.
                                                                                                     // [002A00-002AFF] Supplemental Mathematical Operators.
                                                                                                     // [0027C0-0027EF] Miscellaneous Mathematical Symbols-A.
                                                                                                     // [002980-0029FF] Miscellaneous Mathematical Symbols-B.
    MiscellaneousTechnical              = BL_FONT_UC_INDEX_MISCELLANEOUS_TECHNICAL,                  // [002300-0023FF] Miscellaneous Technical.
    ControlPictures                     = BL_FONT_UC_INDEX_CONTROL_PICTURES,                         // [002400-00243F] Control Pictures.
    OpticalCharacterRecognition         = BL_FONT_UC_INDEX_OPTICAL_CHARACTER_RECOGNITION,            // [002440-00245F] Optical Character Recognition.
    EnclosedAlphanumerics               = BL_FONT_UC_INDEX_ENCLOSED_ALPHANUMERICS,                   // [002460-0024FF] Enclosed Alphanumerics.
    BoxDrawing                          = BL_FONT_UC_INDEX_BOX_DRAWING,                              // [002500-00257F] Box Drawing.
    BlockElements                       = BL_FONT_UC_INDEX_BLOCK_ELEMENTS,                           // [002580-00259F] Block Elements.
    GeometricShapes                     = BL_FONT_UC_INDEX_GEOMETRIC_SHAPES,                         // [0025A0-0025FF] Geometric Shapes.
    MiscellaneousSymbols                = BL_FONT_UC_INDEX_MISCELLANEOUS_SYMBOLS,                    // [002600-0026FF] Miscellaneous Symbols.
    Dingbats                            = BL_FONT_UC_INDEX_DINGBATS,                                 // [002700-0027BF] Dingbats.
    CJKSymbolsAndPunctuation            = BL_FONT_UC_INDEX_CJK_SYMBOLS_AND_PUNCTUATION,              // [003000-00303F] CJK Symbols And Punctuation.
    Hiragana                            = BL_FONT_UC_INDEX_HIRAGANA,                                 // [003040-00309F] Hiragana.
    Katakana                            = BL_FONT_UC_INDEX_KATAKANA,                                 // [0030A0-0030FF] Katakana.
                                                                                                     // [0031F0-0031FF] Katakana Phonetic Extensions.
    Bopomofo                            = BL_FONT_UC_INDEX_BOPOMOFO,                                 // [003100-00312F] Bopomofo.
                                                                                                     // [0031A0-0031BF] Bopomofo Extended.
    HangulCompatibilityJamo             = BL_FONT_UC_INDEX_HANGUL_COMPATIBILITY_JAMO,                // [003130-00318F] Hangul Compatibility Jamo.
    PhagsPa                             = BL_FONT_UC_INDEX_PHAGS_PA,                                 // [00A840-00A87F] Phags-pa.
    EnclosedCJKLettersAndMonths         = BL_FONT_UC_INDEX_ENCLOSED_CJK_LETTERS_AND_MONTHS,          // [003200-0032FF] Enclosed CJK Letters And Months.
    CJKCompatibility                    = BL_FONT_UC_INDEX_CJK_COMPATIBILITY,                        // [003300-0033FF] CJK Compatibility.
    HangulSyllables                     = BL_FONT_UC_INDEX_HANGUL_SYLLABLES,                         // [00AC00-00D7AF] Hangul Syllables.
    NonPlane                            = BL_FONT_UC_INDEX_NON_PLANE,                                // [00D800-00DFFF] Non-Plane 0 *.
    Phoenician                          = BL_FONT_UC_INDEX_PHOENICIAN,                               // [010900-01091F] Phoenician.
    CJKUnifiedIdeographs                = BL_FONT_UC_INDEX_CJK_UNIFIED_IDEOGRAPHS,                   // [004E00-009FFF] CJK Unified Ideographs.
                                                                                                     // [002E80-002EFF] CJK Radicals Supplement.
                                                                                                     // [002F00-002FDF] Kangxi Radicals.
                                                                                                     // [002FF0-002FFF] Ideographic Description Characters.
                                                                                                     // [003400-004DBF] CJK Unified Ideographs Extension A.
                                                                                                     // [020000-02A6DF] CJK Unified Ideographs Extension B.
                                                                                                     // [003190-00319F] Kanbun.
    PrivateUsePlane0                    = BL_FONT_UC_INDEX_PRIVATE_USE_PLANE0,                       // [00E000-00F8FF] Private Use (Plane 0).
    CJKStrokes                          = BL_FONT_UC_INDEX_CJK_STROKES,                              // [0031C0-0031EF] CJK Strokes.
                                                                                                     // [00F900-00FAFF] CJK Compatibility Ideographs.
                                                                                                     // [02F800-02FA1F] CJK Compatibility Ideographs Supplement.
    AlphabeticPresentationForms         = BL_FONT_UC_INDEX_ALPHABETIC_PRESENTATION_FORMS,            // [00FB00-00FB4F] Alphabetic Presentation Forms.
    ArabicPresentationsFormsA           = BL_FONT_UC_INDEX_ARABIC_PRESENTATION_FORMS_A,              // [00FB50-00FDFF] Arabic Presentation Forms-A.
    CombiningHalfMarks                  = BL_FONT_UC_INDEX_COMBINING_HALF_MARKS,                     // [00FE20-00FE2F] Combining Half Marks.
    VerticalForms                       = BL_FONT_UC_INDEX_VERTICAL_FORMS,                           // [00FE10-00FE1F] Vertical Forms.
                                                                                                     // [00FE30-00FE4F] CJK Compatibility Forms.
    SmallFormVariants                   = BL_FONT_UC_INDEX_SMALL_FORM_VARIANTS,                      // [00FE50-00FE6F] Small Form Variants.
    ArabicPresentationFormsB            = BL_FONT_UC_INDEX_ARABIC_PRESENTATION_FORMS_B,              // [00FE70-00FEFF] Arabic Presentation Forms-B.
    HalfwidthAndFullwidthForms          = BL_FONT_UC_INDEX_HALFWIDTH_AND_FULLWIDTH_FORMS,            // [00FF00-00FFEF] Halfwidth And Fullwidth Forms.
    Specials                            = BL_FONT_UC_INDEX_SPECIALS,                                 // [00FFF0-00FFFF] Specials.
    Tibetan                             = BL_FONT_UC_INDEX_TIBETAN,                                  // [000F00-000FFF] Tibetan.
    Syriac                              = BL_FONT_UC_INDEX_SYRIAC,                                   // [000700-00074F] Syriac.
    Thaana                              = BL_FONT_UC_INDEX_THAANA,                                   // [000780-0007BF] Thaana.
    Sinhala                             = BL_FONT_UC_INDEX_SINHALA,                                  // [000D80-000DFF] Sinhala.
    Myanmar                             = BL_FONT_UC_INDEX_MYANMAR,                                  // [001000-00109F] Myanmar.
    Ethiopic                            = BL_FONT_UC_INDEX_ETHIOPIC,                                 // [001200-00137F] Ethiopic.
                                                                                                     // [001380-00139F] Ethiopic Supplement.
                                                                                                     // [002D80-002DDF] Ethiopic Extended.
    Cherokee                            = BL_FONT_UC_INDEX_CHEROKEE,                                 // [0013A0-0013FF] Cherokee.
    UnifiedCanadianAboriginalSyllabics  = BL_FONT_UC_INDEX_UNIFIED_CANADIAN_ABORIGINAL_SYLLABICS,    // [001400-00167F] Unified Canadian Aboriginal Syllabics.
    Ogham                               = BL_FONT_UC_INDEX_OGHAM,                                    // [001680-00169F] Ogham.
    Runic                               = BL_FONT_UC_INDEX_RUNIC,                                    // [0016A0-0016FF] Runic.
    Khmer                               = BL_FONT_UC_INDEX_KHMER,                                    // [001780-0017FF] Khmer.
                                                                                                     // [0019E0-0019FF] Khmer Symbols.
    Mongolian                           = BL_FONT_UC_INDEX_MONGOLIAN,                                // [001800-0018AF] Mongolian.
    BraillePatterns                     = BL_FONT_UC_INDEX_BRAILLE_PATTERNS,                         // [002800-0028FF] Braille Patterns.
    YiSyllablesAndRadicals              = BL_FONT_UC_INDEX_YI_SYLLABLES_AND_RADICALS,                // [00A000-00A48F] Yi Syllables.
                                                                                                     // [00A490-00A4CF] Yi Radicals.
    TagalogHanunooBuhidTagbanwa         = BL_FONT_UC_INDEX_TAGALOG_HANUNOO_BUHID_TAGBANWA,           // [001700-00171F] Tagalog.
                                                                                                     // [001720-00173F] Hanunoo.
                                                                                                     // [001740-00175F] Buhid.
                                                                                                     // [001760-00177F] Tagbanwa.
    OldItalic                           = BL_FONT_UC_INDEX_OLD_ITALIC,                               // [010300-01032F] Old Italic.
    Gothic                              = BL_FONT_UC_INDEX_GOTHIC,                                   // [010330-01034F] Gothic.
    Deseret                             = BL_FONT_UC_INDEX_DESERET,                                  // [010400-01044F] Deseret.
    MusicalSymbols                      = BL_FONT_UC_INDEX_MUSICAL_SYMBOLS,                          // [01D000-01D0FF] Byzantine Musical Symbols.
                                                                                                     // [01D100-01D1FF] Musical Symbols.
                                                                                                     // [01D200-01D24F] Ancient Greek Musical Notation.
    MathematicalAlphanumericSymbols     = BL_FONT_UC_INDEX_MATHEMATICAL_ALPHANUMERIC_SYMBOLS,        // [01D400-01D7FF] Mathematical Alphanumeric Symbols.
    PrivateUsePlane15And16              = BL_FONT_UC_INDEX_PRIVATE_USE_PLANE_15_16,                  // [0F0000-0FFFFD] Private Use (Plane 15).
                                                                                                     // [100000-10FFFD] Private Use (Plane 16).
    VariationSelectors                  = BL_FONT_UC_INDEX_VARIATION_SELECTORS,                      // [00FE00-00FE0F] Variation Selectors.
                                                                                                     // [0E0100-0E01EF] Variation Selectors Supplement.
    Tags                                = BL_FONT_UC_INDEX_TAGS,                                     // [0E0000-0E007F] Tags.
    Limbu                               = BL_FONT_UC_INDEX_LIMBU,                                    // [001900-00194F] Limbu.
    TaiLe                               = BL_FONT_UC_INDEX_TAI_LE,                                   // [001950-00197F] Tai Le.
    NewTaiLue                           = BL_FONT_UC_INDEX_NEW_TAI_LUE,                              // [001980-0019DF] New Tai Lue.
    Buginese                            = BL_FONT_UC_INDEX_BUGINESE,                                 // [001A00-001A1F] Buginese.
    Glagolitic                          = BL_FONT_UC_INDEX_GLAGOLITIC,                               // [002C00-002C5F] Glagolitic.
    Tifinagh                            = BL_FONT_UC_INDEX_TIFINAGH,                                 // [002D30-002D7F] Tifinagh.
    YijingHexagramSymbols               = BL_FONT_UC_INDEX_YIJING_HEXAGRAM_SYMBOLS,                  // [004DC0-004DFF] Yijing Hexagram Symbols.
    SylotiNagri                         = BL_FONT_UC_INDEX_SYLOTI_NAGRI,                             // [00A800-00A82F] Syloti Nagri.
    LinearBSyllabaryAndIdeograms        = BL_FONT_UC_INDEX_LINEAR_B_SYLLABARY_AND_IDEOGRAMS,         // [010000-01007F] Linear B Syllabary.
                                                                                                     // [010080-0100FF] Linear B Ideograms.
                                                                                                     // [010100-01013F] Aegean Numbers.
    AncientGreekNumbers                 = BL_FONT_UC_INDEX_ANCIENT_GREEK_NUMBERS,                    // [010140-01018F] Ancient Greek Numbers.
    Ugaritic                            = BL_FONT_UC_INDEX_UGARITIC,                                 // [010380-01039F] Ugaritic.
    OldPersian                          = BL_FONT_UC_INDEX_OLD_PERSIAN,                              // [0103A0-0103DF] Old Persian.
    Shavian                             = BL_FONT_UC_INDEX_SHAVIAN,                                  // [010450-01047F] Shavian.
    Osmanya                             = BL_FONT_UC_INDEX_OSMANYA,                                  // [010480-0104AF] Osmanya.
    CypriotSyllabary                    = BL_FONT_UC_INDEX_CYPRIOT_SYLLABARY,                        // [010800-01083F] Cypriot Syllabary.
    Kharoshthi                          = BL_FONT_UC_INDEX_KHAROSHTHI,                               // [010A00-010A5F] Kharoshthi.
    TaiXuanJingSymbols                  = BL_FONT_UC_INDEX_TAI_XUAN_JING_SYMBOLS,                    // [01D300-01D35F] Tai Xuan Jing Symbols.
    Cuneiform                           = BL_FONT_UC_INDEX_CUNEIFORM,                                // [012000-0123FF] Cuneiform.
                                                                                                     // [012400-01247F] Cuneiform Numbers and Punctuation.
    CountingRodNumerals                 = BL_FONT_UC_INDEX_COUNTING_ROD_NUMERALS,                    // [01D360-01D37F] Counting Rod Numerals.
    Sundanese                           = BL_FONT_UC_INDEX_SUNDANESE,                                // [001B80-001BBF] Sundanese.
    Lepcha                              = BL_FONT_UC_INDEX_LEPCHA,                                   // [001C00-001C4F] Lepcha.
    OlChiki                             = BL_FONT_UC_INDEX_OL_CHIKI,                                 // [001C50-001C7F] Ol Chiki.
    Saurashtra                          = BL_FONT_UC_INDEX_SAURASHTRA,                               // [00A880-00A8DF] Saurashtra.
    KayahLi                             = BL_FONT_UC_INDEX_KAYAH_LI,                                 // [00A900-00A92F] Kayah Li.
    Rejang                              = BL_FONT_UC_INDEX_REJANG,                                   // [00A930-00A95F] Rejang.
    Cham                                = BL_FONT_UC_INDEX_CHAM,                                     // [00AA00-00AA5F] Cham.
    AncientSymbols                      = BL_FONT_UC_INDEX_ANCIENT_SYMBOLS,                          // [010190-0101CF] Ancient Symbols.
    PhaistosDisc                        = BL_FONT_UC_INDEX_PHAISTOS_DISC,                            // [0101D0-0101FF] Phaistos Disc.
    CarianLycianLydian                  = BL_FONT_UC_INDEX_CARIAN_LYCIAN_LYDIAN,                     // [0102A0-0102DF] Carian.
                                                                                                     // [010280-01029F] Lycian.
                                                                                                     // [010920-01093F] Lydian.
    DominoAndMahjongTiles               = BL_FONT_UC_INDEX_DOMINO_AND_MAHJONG_TILES,                 // [01F030-01F09F] Domino Tiles.
                                                                                                     // [01F000-01F02F] Mahjong Tiles.
    InternalUsage123                    = BL_FONT_UC_INDEX_INTERNAL_USAGE_123,                       // Reserved for internal usage (123).
    InternalUsage124                    = BL_FONT_UC_INDEX_INTERNAL_USAGE_124,                       // Reserved for internal usage (124).
    InternalUsage125                    = BL_FONT_UC_INDEX_INTERNAL_USAGE_125,                       // Reserved for internal usage (125).
    InternalUsage126                    = BL_FONT_UC_INDEX_INTERNAL_USAGE_126,                       // Reserved for internal usage (126).
    InternalUsage127                    = BL_FONT_UC_INDEX_INTERNAL_USAGE_127);                      // Reserved for internal usage (127).

type
  { Text direction. }
  TBLTextDirection = (
    { Left-to-right direction. }
    LTR = BL_TEXT_DIRECTION_LTR,

    { Right-to-left direction. }
    RTL = BL_TEXT_DIRECTION_RTL);

type
  { Text orientation. }
  TBLTextOrientation = (
    { Horizontal orientation. }
    Horizontal = BL_TEXT_ORIENTATION_HORIZONTAL,

    { Vertical orientation. }
    Vertical   = BL_TEXT_ORIENTATION_VERTICAL);

{ ============================================================================
   [BLGlyphInfo]
  ============================================================================ }

type
  { Contains additional information associated with a glyph used by
    IBLGlyphBuffer. }
  TBLGlyphInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGlyphInfo;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property Cluster: Cardinal read FHandle.cluster write FHandle.cluster;
  end;
  PBLGlyphInfo = ^TBLGlyphInfo;

{ ============================================================================
   [BLGlyphPlacement]
  ============================================================================ }

type
  { Glyph placement.

    Provides information about glyph offset (x/y) and advance (x/y). }
  TBLGlyphPlacement = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGlyphPlacement;
    function GetPlacement: TBLPointI; inline;
    procedure SetPlacement(const AValue: TBLPointI); inline;
    function GetAdvance: TBLPointI; inline;
    procedure SetAdvance(const AValue: TBLPointI); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property Placement: TBLPointI read GetPlacement write SetPlacement;
    property Advance: TBLPointI read GetAdvance write SetAdvance;
  end;
  PBLGlyphPlacement = ^TBLGlyphPlacement;

{ ============================================================================
   [BLGlyphMappingState]
  ============================================================================ }

type
  { Character to glyph mapping state. }
  TBLGlyphMappingState = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGlyphMappingState;
    function GetGlyphCount: Integer; inline;
    procedure SetGlyphCount(const AValue: Integer); inline;
    function GetUndefinedFirst: Integer; inline;
    procedure SetUndefinedFirst(const AValue: Integer); inline;
    function GetUndefinedCount: Integer; inline;
    procedure SetUndefinedCount(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Number of glyphs or glyph-items on output. }
    property GlyphCount: Integer read GetGlyphCount write SetGlyphCount;

    { Index of the first undefined glyph (-1 if none). }
    property UndefinedFirst: Integer read GetUndefinedFirst write SetUndefinedFirst;

    { Undefined glyph count (chars that have no mapping). }
    property UndefinedCount: Integer read GetUndefinedCount write SetUndefinedCount;
  end;
  PBLGlyphMappingState = ^TBLGlyphMappingState;

{ ============================================================================
   [BLGlyphOutlineSinkInfo]
  ============================================================================ }

type
  { Information passed to a TBLPathSinkEvent sink by IBLFont.GetGlyphOutlines. }
  TBLGlyphOutlineSinkInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGlyphOutlineSinkInfo;
    function GetGlyphIndex: Integer; inline;
    procedure SetGlyphIndex(const AValue: Integer); inline;
    function GetContourCount: Integer; inline;
    procedure SetContourCount(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property GlyphIndex: Integer read GetGlyphIndex write SetGlyphIndex;
    property ContourCount: Integer read GetContourCount write SetContourCount;
  end;
  PBLGlyphOutlineSinkInfo = ^TBLGlyphOutlineSinkInfo;

{ ============================================================================
   [BLGlyphRun]
  ============================================================================ }

type
  { An entry enumerated by TBLGlyphRun.GetEnumerator }
  TBLGlyphRunEntry<TPlacement: record> = record
  public
    { The glyph Id }
    GlyphId: Cardinal;

    { Glyph placement (type should match TBLGlyphRun.PlacementType) }
    Placement: TPlacement;
  end;

type
  { TBLGlyphRun describes a set of consecutive glyphs and their placements.

    TBLGlyphRun should only be used to pass glyph IDs and their placements to
    the rendering context. The purpose of TBLGlyphRun is to allow rendering
    glyphs, which could be shaped by various shaping engines (Blend2D, Harfbuzz,
    etc).

    TBLGlyphRun allows to render glyphs that are either stored in TArray<UInt16>
    or TArray<UInt32> or part of a bigger structure (for example hb_glyph_info_t
    used by HarfBuzz). Glyph placements at the moment use Blend2D's
    TBLGlyphPlacement or TBLPoint, but it's possible to extend the data type in
    the future.

    See TBLGlyphRunPlacement for placement modes provided by Blend2D. }
  TBLGlyphRun = record
  {$REGION 'Internal Declarations'}
  private type
    TEnumerator<T: record> = record
    private type
      P = ^T;
    private
      FGlyphData: PByte;
      FPlacementData: PByte;
      FGlyphAdvance: Integer;
      FPlacementAdvance: Integer;
      FHigh: Integer;
      FIndex: Integer;
      function GetCurrent: TBLGlyphRunEntry<T>; inline;
    public
      constructor Create(const AHandle: _PBLGlyphRun);
      function MoveNext: Boolean; inline;

      property Current: TBLGlyphRunEntry<T> read GetCurrent;
    end;
  private type
    TEnumerable<T: record> = record
    private
      FHandle: _PBLGlyphRun;
    public
      constructor Create(const AHandle: _PBLGlyphRun);
      function GetEnumerator: TEnumerator<T>;
    end;
  private
    FHandle: BLGlyphRun;
    function GetSize: Integer; inline;
    procedure SetSize(const AValue: Integer); inline;
    function GetPlacementType: TBLGlyphPlacementType; inline;
    procedure SetPlacementType(const AValue: TBLGlyphPlacementType); inline;
    function GetFlags: TBLGlyphRunFlags; inline;
    procedure SetFlags(const AValue: TBLGlyphRunFlags); inline;
    function GetIsEmpty: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;
    procedure SetGlyphData(const AData: PWord); overload; inline;
    procedure SetGlyphData(const AData: PCardinal); overload; inline;
    procedure SetGlyphData(const AData: Pointer; const AAdvance: Integer); overload; inline;
    procedure ResetGlyphIdData; inline;
    procedure SetPlacementData(const AData: Pointer; const AAdvance: Integer); inline;
    procedure ResetPlacementData; inline;

    { Support for..in enumeration of the glyphs in the run.
      The type parameter TPlacement of each entry should be a type that is
      compatible with the PlacementType property.

      Example:

      var
        GlyphRun: TBLGlyphRun;
        Entry: TBLGlyphRunEntry<TBLPoint>;
      begin
        GlyphRun := ...;
        for Entry in GlyphRun.Entries<TBLPoint> do
          ...
      end; }
    function Entries<TPlacement: record>: TEnumerable<TPlacement>;

    { Glyph id data (abstract, incremented by GlyphAdvance). }
    property GlyphData: Pointer read FHandle.glyphData write FHandle.glyphData;

    { Glyph placement data (abstract, incremented by PlacementAdvance). }
    property PlacementData: Pointer read FHandle.placementData write FHandle.placementData;

    { Size of the glyph-run in glyph units. }
    property Size: Integer read GetSize write SetSize;

    { Size of a GlyphId - must be either 2 (UInt16) or 4 (UInt32) bytes.

      Blend2D always uses 32-bit glyph-ids, thus the glyph-run returned
      by IBLGlyphBuffer has always set GlyphSize to 4. The possibility to
      render glyphs of size 2 is strictly for compatibility with text shapers
      that use 16-bit glyphs, which is sufficient for TrueType and OpenType
      fonts.}
    property GlyphSize: Byte read FHandle.glyphSize write FHandle.glyphSize;

    { Type of placement }
    property PlacementType: TBLGlyphPlacementType read GetPlacementType write SetPlacementType;

    { Advance of GlyphData array. }
    property GlyphAdvance: Shortint read FHandle.glyphAdvance write FHandle.glyphAdvance;

    { Glyph-run flags. }
    property Flags: TBLGlyphRunFlags read GetFlags write SetFlags;

    property IsEmpty: Boolean read GetIsEmpty;
  end;
  PBLGlyphRun = ^TBLGlyphRun;

{ ============================================================================
   [BLFontFaceInfo]
  ============================================================================ }

type
  { Information of IBLFontFace. }
  TBLFontFaceInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontFaceInfo;
    function GetFaceType: TBLFontFaceType; inline;
    procedure SetFaceType(const AValue: TBLFontFaceType); inline;
    function GetOutlineType: TBLFontOutlineType; inline;
    procedure SetOutlineType(const AValue: TBLFontOutlineType); inline;
    function GetGlyphCount: Integer; inline;
    procedure SetGlyphCount(const AValue: Integer); inline;
    function GetFaceFlags: TBLFontFaceFlags; inline;
    procedure SetFaceFlags(const AValue: TBLFontFaceFlags); inline;
    function GetDiagFlags: TBLFontFaceDiagFlags; inline;
    procedure SetDiagFlags(const AValue: TBLFontFaceDiagFlags); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Font-face type }
    property FaceType: TBLFontFaceType read GetFaceType write SetFaceType;

    { Type of outlines used by the font-face }
    property OutlineType: TBLFontOutlineType read GetOutlineType write SetOutlineType;

    { Number of glyphs provided by this font-face. }
    property GlyphCount: Integer read GetGlyphCount write SetGlyphCount;

    { Revision (read from 'head' table, represented as 16.16 fixed point). }
    property Revision: Cardinal read FHandle.revision write FHandle.revision;

    { Face-face index in a TTF/OTF collection or zero if not part of a collection. }
    property FaceIndex: Integer read FHandle.faceIndex write FHandle.faceIndex;

    { Font-face flags }
    property FaceFlags: TBLFontFaceFlags read GetFaceFlags write SetFaceFlags;

    { Font-face diagnostic flags }
    property DiagFlags: TBLFontFaceDiagFlags read GetDiagFlags write SetDiagFlags;
  end;
  PBLFontFaceInfo = ^TBLFontFaceInfo;

{ ============================================================================
   [BLFontQueryProperties]
  ============================================================================ }

type
  { Properties that can be used to query IBLFont and IBLFontFace. }
  TBLFontQueryProperties = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontQueryProperties;
    function GetStyle: TBLFontStyle; inline;
    procedure SetStyle(const AValue: TBLFontStyle); inline;
    function GetWeight: TBLFontWeight; inline;
    procedure SetWeight(const AValue: TBLFontWeight); inline;
    function GetStretch: TBLFontStretch; inline;
    procedure SetStretch(const AValue: TBLFontStretch); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Font style }
    property Style: TBLFontStyle read GetStyle write SetStyle;

    { Font weight }
    property Weight: TBLFontWeight read GetWeight write SetWeight;

    { Font stretch }
    property Stretch: TBLFontStretch read GetStretch write SetStretch;
  end;

{ ============================================================================
   [BLFontTable]
  ============================================================================ }

type
  { A read only data that represents a font table or its sub-table. }
  TBLFontTable = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontTable;
    function GetSize: Integer; inline;
    procedure SetSize(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AData: Pointer; const ASize: Integer); overload; inline;

    { Pointer to the beginning of the data }
    property Data: Pointer read FHandle.data write FHandle.data;

    { Size of Data in bytes. }
    property Size: Integer read GetSize write SetSize;
  end;
  PBLFontTable = ^TBLFontTable;

{ ============================================================================
   [BLFontFeature]
  ============================================================================ }

type
  { Associates a value with a generic font feature where Tag describes the
    feature (as provided by the font) and Value describes its value. Some
    features only allow boolean values 0 and 1 and some also allow higher
    values up to 65535.

    Registered OpenType features:
    - https://docs.microsoft.com/en-us/typography/opentype/spec/featuretags
    - https://helpx.adobe.com/typekit/using/open-type-syntax.html }
  TBLFontFeature = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontFeature;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Feature tag (32-bit). }
    property Tag: TBLTag read FHandle.tag write FHandle.tag;

    { Feature value (should not be greater than 65535). }
    property Value: Cardinal read FHandle.value write FHandle.value;
  end;
  PBLFontFeature = ^TBLFontFeature;

{ ============================================================================
   [BLFontVariation]
  ============================================================================ }

type
  { Associates a value with a font variation feature where Tag describes
    variation axis and Value defines its value. }
  TBLFontVariation = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontVariation;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Variation tag (32-bit). }
    property Tag: TBLTag read FHandle.tag write FHandle.tag;

    { Variation value. }
    property Value: Single read FHandle.value write FHandle.value;
  end;
  PBLFontVariation = ^TBLFontVariation;

{ ============================================================================
   [BLFontUnicodeCoverage]
  ============================================================================ }

type
  { Font unicode coverage.

    Unicode coverage describes which unicode characters are provided by a font.
    Blend2D accesses this information by reading "OS/2" table, if available. }
  TBLFontUnicodeCoverage = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontUnicodeCoverage;
    function GetIsEmpty: Boolean; inline;
    function GetBit(const AIndex: TBLFontUnicodeCoverageIndex): Boolean;
    procedure SetBit(const AIndex: TBLFontUnicodeCoverageIndex; const AValue: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLFontUnicodeCoverage): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLFontUnicodeCoverage): Boolean; inline; static;
  public
    procedure Reset; inline;
    procedure SetBit(const AIndex: TBLFontUnicodeCoverageIndex); overload; inline;
    procedure ClearBit(const AIndex: TBLFontUnicodeCoverageIndex); inline;

    property IsEmpty: Boolean read GetIsEmpty;
    property Bits[const AIndex: TBLFontUnicodeCoverageIndex]: Boolean read GetBit write SetBit;
  end;
  PBLFontUnicodeCoverage = ^TBLFontUnicodeCoverage;

{ ============================================================================
   [BLFontPanose]
  ============================================================================ }

type
  TBLFontPanoseText = record
    FamilyKind: Byte;
    SerifStyle: Byte;
    Weight: Byte;
    Proportion: Byte;
    Contrast: Byte;
    StrokeVariation: Byte;
    ArmStyle: Byte;
    Letterform: Byte;
    Midline: Byte;
    XHeight: Byte
  end;

type
  TBLFontPanoseScript = record
    FamilyKind: Byte;
    ToolKind: Byte;
    Weight: Byte;
    Spacing: Byte;
    AspectRatio: Byte;
    Contrast: Byte;
    Topology: Byte;
    Form: Byte;
    Finials: Byte;
    XAscent: Byte;
  end;

type
  TBLFontPanoseDecorative = record
    FamilyKind: Byte;
    DecorativeClass: Byte;
    Weight: Byte;
    Aspect: Byte;
    Contrast: Byte;
    SerifVariant: Byte;
    Treatment: Byte;
    Lining: Byte;
    Topology: Byte;
    CharacterRange: Byte;
  end;

type
  TBLFontPanoseSymbol = record
    FamilyKind: Byte;
    SymbolKind: Byte;
    Weight: Byte;
    Spacing: Byte;
    AspectRatioAndContrast: Byte;
    AspectRatio94: Byte;
    AspectRatio119: Byte;
    AspectRatio157: Byte;
    AspectRatio163: Byte;
    AspectRatio211: Byte;
  end;

type
  { Scaled TBLFontDesignMetrics based on font size and other properties. }
  TBLFontPanose = record
  {$REGION 'Internal Declarations'}
  private
    function GetIsEmpty: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property IsEmpty: Boolean read GetIsEmpty;
  public
    case Integer of
      0: (Data: array [0..9] of Byte);
      1: (FamilyKind: Byte);
      2: (Text: TBLFontPanoseText);
      3: (Script: TBLFontPanoseScript);
      4: (Decorative: TBLFontPanoseDecorative);
      5: (Symbol: TBLFontPanoseSymbol);
  end;
  PBLFontPanose = ^TBLFontPanose;

{ ============================================================================
   [BLFontMatrix]
  ============================================================================ }

type
  { 2x2 transformation matrix used by IBLFont. It's similar to TBLMatrix2D,
    however, it doesn't provide a translation part as it's assumed to be zero. }
  TBLFontMatrix = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontMatrix;
    function GetElement(const AIndex: Integer): Double; inline;
    procedure SetElement(const AIndex: Integer; const AValue: Double); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const AM00, AM01, AM10, AM11: Double); overload; inline;

    property Elements[const AIndex: Integer]: Double read GetElement write SetElement; default;
    property M00: Double read FHandle.m00 write FHandle.m00;
    property M01: Double read FHandle.m01 write FHandle.m01;
    property M10: Double read FHandle.m10 write FHandle.m10;
    property M11: Double read FHandle.m11 write FHandle.m11;
  end;
  PBLFontMatrix = ^TBLFontMatrix;

function BLFontMatrix(const AM00, AM01, AM10, AM11: Double): TBLFontMatrix; inline;

{ ============================================================================
   [BLFontMetrics]
  ============================================================================ }

type
  { Scaled TBLFontDesignMetrics based on font size and other properties. }
  TBLFontMetrics = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontMetrics;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Font ascent (horizontal orientation). }
    property Ascent: Single read FHandle.ascent write FHandle.ascent;

    { Font ascent (vertical orientation). }
    property VAscent: Single read FHandle.vAscent write FHandle.vAscent;

    { Font descent (horizontal orientation). }
    property Descent: Single read FHandle.descent write FHandle.descent;

    { Font descent (vertical orientation). }
    property VDescent: Single read FHandle.vDescent write FHandle.vDescent;

    { Line gap. }
    property LineGap: Single read FHandle.lineGap write FHandle.lineGap;

    { Distance between the baseline and the mean line of lower-case letters. }
    property XHeight: Single read FHandle.xHeight write FHandle.xHeight;

    { Maximum height of a capital letter above the baseline.}
    property CapHeight: Single read FHandle.capHeight write FHandle.capHeight;

    { Minimum x, reported by the font. }
    property XMin: Single read FHandle.xMin write FHandle.xMin;

    { Minimum y, reported by the font. }
    property YMin: Single read FHandle.yMin write FHandle.yMin;

    { Maximum x, reported by the font. }
    property XMax: Single read FHandle.xMax write FHandle.xMax;

    { Maximum y, reported by the font. }
    property YMax: Single read FHandle.yMax write FHandle.yMax;

    { Text underline position. }
    property UnderlinePosition: Single read FHandle.underlinePosition write FHandle.underlinePosition;

    { Text underline thickness. }
    property UnderlineThickness: Single read FHandle.underlineThickness write FHandle.underlineThickness;

    { Text strikethrough position. }
    property StrikethroughPosition: Single read FHandle.strikethroughPosition write FHandle.strikethroughPosition;

    { Text strikethrough thickness. }
    property StrikethroughThickness: Single read FHandle.strikethroughThickness write FHandle.strikethroughThickness;
  end;
  PBLFontMetrics = ^TBLFontMetrics;

{ ============================================================================
   [BLFontDesignMetrics]
  ============================================================================ }

type
  { Design metrics of a font.

    Design metrics is information that IBLFontFace collected directly from the
    font data. It means that all fields are measured in font design units.

    When a new IBLFont instance is created a scaled metrics IBLFontMetrics is
    automatically calculated from IBLFontDesignMetrics including other members
    like transformation, etc... }
  TBLFontDesignMetrics = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontDesignMetrics;
    function GetGlyphBoundingBox: TBLBoxI; inline;
    procedure SetGlyphBoundingBox(const AValue: TBLBoxI); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Units per EM square. }
    property UnitsPerEm: Integer read FHandle.unitsPerEm write FHandle.unitsPerEm;

    { Lowest readable size in pixels. }
    property LowestPPEM: Integer read FHandle.lowestPPEM write FHandle.lowestPPEM;

    { Line gap. }
    property LineGap: Integer read FHandle.lineGap write FHandle.lineGap;

    { Distance between the baseline and the mean line of lower-case letters. }
    property XHeight: Integer read FHandle.xHeight write FHandle.xHeight;

    { Maximum height of a capital letter above the baseline. }
    property CapHeight: Integer read FHandle.capHeight write FHandle.capHeight;

    { Ascent (horizontal layout). }
    property Ascent: Integer read FHandle.ascent write FHandle.ascent;

    { Ascent (vertical layout). }
    property VAscent: Integer read FHandle.vAscent write FHandle.vAscent;

    { Descent (horizontal layout). }
    property Descent: Integer read FHandle.descent write FHandle.descent;

    { Descent (vertical layout). }
    property VDescent: Integer read FHandle.vDescent write FHandle.vDescent;

    { Minimum leading-side bearing (horizontal layout). }
    property HMinLSB: Integer read FHandle.hMinLSB write FHandle.hMinLSB;

    { Minimum leading-side bearing (vertical layout). }
    property VMinLSB: Integer read FHandle.vMinLSB write FHandle.vMinLSB;

    { Minimum trailing-side bearing (horizontal layout). }
    property HMinTSB: Integer read FHandle.hMinTSB write FHandle.hMinTSB;

    { Minimum trailing-side bearing (vertical layout). }
    property VMinTSB: Integer read FHandle.vMinTSB write FHandle.vMinTSB;

    { Maximum advance (horizontal layout). }
    property HMaxAdvance: Integer read FHandle.hMaxAdvance write FHandle.hMaxAdvance;

    { Maximum advance (vertical layout). }
    property VMaxAdvance: Integer read FHandle.vMaxAdvance write FHandle.vMaxAdvance;

    { Aggregated bounding box of all glyphs in the font.
      This value is reported by the face so it's not granted to be true. }
    property GlyphBoundingBox: TBLBoxI read GetGlyphBoundingBox write SetGlyphBoundingBox;

    { Minimum x, reported by the font. }
    property XMin: Integer read FHandle.glyphBoundingBox.x0 write FHandle.glyphBoundingBox.x0;

    { Minimum y, reported by the font. }
    property YMin: Integer read FHandle.glyphBoundingBox.y0 write FHandle.glyphBoundingBox.y0;

    { Maximum x, reported by the font. }
    property XMax: Integer read FHandle.glyphBoundingBox.x1 write FHandle.glyphBoundingBox.x1;

    { Maximum y, reported by the font. }
    property YMax: Integer read FHandle.glyphBoundingBox.y1 write FHandle.glyphBoundingBox.y1;


    { Text underline position. }
    property UnderlinePosition: Integer read FHandle.underlinePosition write FHandle.underlinePosition;

    { Text underline thickness. }
    property UnderlineThickness: Integer read FHandle.underlineThickness write FHandle.underlineThickness;

    { Text strikethrough position. }
    property StrikethroughPosition: Integer read FHandle.strikethroughPosition write FHandle.strikethroughPosition;

    { Text strikethrough thickness. }
    property StrikethroughThickness: Integer read FHandle.strikethroughThickness write FHandle.strikethroughThickness;
  end;
  PBLFontDesignMetrics = ^TBLFontDesignMetrics;

{ ============================================================================
   [BLTextMetrics]
  ============================================================================ }

type
  { Text metrics. }
  TBLTextMetrics = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLTextMetrics;
    function GetAdvance: TBLPoint; inline;
    procedure SetAdvance(const AValue: TBLPoint); inline;
    function GetLeadingBearing: TBLPoint; inline;
    procedure SetLeadingBearing(const AValue: TBLPoint); inline;
    function GetTrailingBearing: TBLPoint; inline;
    procedure SetTrailingBearing(const AValue: TBLPoint); inline;
    function GetBoundingBox: TBLBox; inline;
    procedure SetBoundingBox(const AValue: TBLBox); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    property Advance: TBLPoint read GetAdvance write SetAdvance;
    property LeadingBearing: TBLPoint read GetLeadingBearing write SetLeadingBearing;
    property TrailingBearing: TBLPoint read GetTrailingBearing write SetTrailingBearing;
    property BoundingBox: TBLBox read GetBoundingBox write SetBoundingBox;
  end;
  PBLTextMetrics = ^TBLTextMetrics;

{$ENDREGION 'Font Defs'}

{$REGION 'Glyph Buffer'}

{ ============================================================================
   [BLGlyphBuffer]
  ============================================================================ }

type
  { Glyph buffer.

    Can hold either text or glyphs and provides basic memory management that is
    used for text shaping, character to glyph mapping, glyph substitution, and
    glyph positioning.

    Glyph buffer provides two separate buffers called 'primary' and 'secondary'
    that serve different purposes during processing. Primary buffer always holds
    actual text/glyph array, and secondary buffer is either used as a scratch
    buffer during glyph substitution or to hold glyph positions after the
    processing is complete and glyph positions were calculated. }
  IBLGlyphBuffer = interface
  ['{0B892B22-C3B5-434D-854C-FBD0FD431B7B}']
    {$REGION 'Internal Declarations'}
    function GetIsEmpty: Boolean;
    function GetSize: Integer;
    function GetFlags: TBLGlyphRunFlags;
    function GetContent: PCardinal;
    function GetInfoData: PBLGlyphInfo;
    function GetPlacementData: PBLGlyphPlacement;
    function GetGlyphRun: PBLGlyphRun;
    function GetHasText: Boolean;
    function GetHasGlyphs: Boolean;
    function GetHasInvalidChars: Boolean;
    function GetHasUndefinedChars: Boolean;
    function GetHasInvalidFontData: Boolean;
    function GetHandle: PBLGlyphBufferCore;
    {$ENDREGION 'Internal Declarations'}

    { Resets the IBLGlyphBuffer into its construction state. The content will
      be cleared and allocated memory released. }
    procedure Reset;

    { Clears the content of IBLGlyphBuffer without releasing internal buffers. }
    procedure Clear;

    { Assigns a text content of this IBLGlyphBuffer. }
    procedure SetText(const AText: String); overload;
    procedure SetText(const AText: UTF8String); overload;
    procedure SetText(const AText: UCS4String); overload;

    { Assigns a text content of this IBLGlyphBuffer.

      This is a generic function that accepts Pointer data, which is specified
      by AEncoding. The ALength argument depends on encoding as well. If the
      encoding specifies byte string (Latin1 or UTF8) then it's bytes, if the
      encoding specifies UTF16 or UTF32 then it would describe the number of
      UInt16 or UInt32 code points, respectively. }
    procedure SetText(const AText: Pointer; const ALength: Integer;
      const AEncoding: TBLTextEncoding); overload;

    { Assigns glyph content of this IBLGlyphBuffer from the given AGlyphData. }
    procedure SetGlyphs(const AGlyphData: TArray<Cardinal>); overload;
    procedure SetGlyphs(const AGlyphData: PCardinal; const ALength: Integer); overload;

    { Assigns glyph content of this IBLGlyphBuffer`from an array of glyphs or
      from a foreign record that contains glyphs and possibly other members that
      have to be skipped. The AGlyphIdSize can be either 16-bit (2) or 32-bit (4).
      The last parameter AGlyphAdvance specifies how many bytes to advance after
      a glyph value is read. }
    procedure SetGlyphs(const AGlyphData: Pointer; const ASize: Integer;
      const AGlyphIdSize, AGlyphAdvance: Integer); overload;

    { Tests whether the glyph-buffer has AFlag set. }
    function HasFlag(const AFlag: TBLGlyphRunFlag): Boolean;

    property IsEmpty: Boolean read GetIsEmpty;
    property Size: Integer read GetSize;
    property Flags: TBLGlyphRunFlags read GetFlags;
    property Content: PCardinal read GetContent;
    property InfoData: PBLGlyphInfo read GetInfoData;
    property PlacementData: PBLGlyphPlacement read GetPlacementData;
    property GlyphRun: PBLGlyphRun read GetGlyphRun;

    { Tests whether the buffer contains unicode data. }
    property HasText: Boolean read GetHasText;

    { Tests whether the buffer contains glyph-id data. }
    property HasGlyphs: Boolean read GetHasGlyphs;

    { Tests whether the input string contained invalid characters (unicode
      encoding errors). }
    property HasInvalidChars: Boolean read GetHasInvalidChars;

    { Tests whether the input string contained undefined characters that weren't
      mapped properly to glyphs. }
    property HasUndefinedChars: Boolean read GetHasUndefinedChars;

    { Tests whether one or more operation was terminated before completion
      because of invalid data in a font. }
    property HasInvalidFontData: Boolean read GetHasInvalidFontData;

    { Internal handle for use with the C API }
    property Handle: PBLGlyphBufferCore read GetHandle;
  end;

type
  { Implements IBLGlyphBuffer }
  TBLGlyphBuffer = class(TInterfacedObject, IBLGlyphBuffer)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLGlyphBufferCore;
  protected
    { IBLGlyphBuffer }
    function GetIsEmpty: Boolean;
    function GetSize: Integer;
    function GetFlags: TBLGlyphRunFlags;
    function GetContent: PCardinal;
    function GetInfoData: PBLGlyphInfo;
    function GetPlacementData: PBLGlyphPlacement;
    function GetGlyphRun: PBLGlyphRun;
    function GetHasText: Boolean;
    function GetHasGlyphs: Boolean;
    function GetHasInvalidChars: Boolean;
    function GetHasUndefinedChars: Boolean;
    function GetHasInvalidFontData: Boolean;
    function GetHandle: PBLGlyphBufferCore;

    procedure Reset;
    procedure Clear;

    procedure SetText(const AText: String); overload;
    procedure SetText(const AText: UTF8String); overload;
    procedure SetText(const AText: UCS4String); overload;
    procedure SetText(const AText: Pointer; const ALength: Integer;
      const AEncoding: TBLTextEncoding); overload;

    procedure SetGlyphs(const AGlyphData: TArray<Cardinal>); overload;
    procedure SetGlyphs(const AGlyphData: PCardinal; const ALength: Integer); overload;
    procedure SetGlyphs(const AGlyphData: Pointer; const ASize: Integer;
      const AGlyphIdSize, AGlyphAdvance: Integer); overload;

    function HasFlag(const AFlag: TBLGlyphRunFlag): Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
  end;

{$ENDREGION 'Glyph Buffer'}

{$REGION 'Font'}

{ ============================================================================
   [BLFontData]
  ============================================================================ }

type
  IBLFontData = interface;

  { This event is called when IBLFontData.InitializeFromData is used and the
    font data is destroyed. }
  TBLFontDataDestroyEvent = procedure (const AFontData: IBLFontData) of object;

  { Font data }
  IBLFontData = interface
  ['{988C31B8-A00C-4BD0-98D9-FDF2EF6D8CAA}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetFaceType: TBLFontFaceType;
    function GetFaceCount: Integer;
    function GetFlags: TBLFontDataFlags;
    function GetIsCollection: Boolean;
    function GetHandle: PBLFontDataCore;
    {$ENDREGION 'Internal Declarations'}

    { Initializes an IBLFontData from a file specified by the given AFilename.

      The AReadFlags argument allows to specify flags that will be passed to
      IBLFileSystem.ReadFile to read the content of the file. It's possible to
      use memory mapping to get its content, which is the recommended way for
      reading system fonts. The best combination is to use the
      TBLFileReadFlag.MmapEnabled flag combined with
      TBLFileReadFlag.MmapAvoidSmall. This combination means to try to use
      memory mapping only when the size of the font is greater than a minimum
      value (determined by Blend2D), and would fallback to a regular open/read
      in case the memory mapping is not possible or failed for some other
      reason. Please note that not all files can be memory mapped so the
      TBLFileReadFlag.MmapNoFallback flag is not recommended. }
    procedure InitializeFromFile(const AFilename: String;
      const AReadFlags: TBLFileReadFlags = []);

    { Initializes an IBLFontData from the given AData.

      The given AData would be weak copied on success so the given array can be
      safely destroyed after the function returns.

      The weak copy of the passed AData is internal and there is no API to
      access it after the function returns. The reason for making it internal
      is that multiple implementations of IBLFontData may exist and some can
      only store data at table level, so Blend2D doesn't expose the detail about
      how the data is stored. }
    procedure InitializeFromData(const AData: TBytes); overload;

    { Creates IBLFontData from the given AData of the given ASize.

      AData must stay alive as long as this object is alive.
      Optionally an AOnDestroy event can be used as a notifier that will be
      called when the data is no longer needed. }
    procedure InitializeFromData(const AData: Pointer; const ASize: Integer;
      const AOnDestroy: TBLFontDataDestroyEvent = nil); overload;

    procedure Reset;
    function Equals(const AOther: IBLFontData): Boolean;

    function GetTags(const AFaceIndex: Integer): TArray<TBLTag>;
    function QueryTable(const AFaceIndex: Integer; const ATag: TBLTag;
      out ATable: TBLFontTable): Integer;
    function QueryTables(const AFaceIndex: Integer; const ATags: TArray<TBLTag>;
      out ATables: TArray<TBLFontTable>): Integer;

    { Whether the font data is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the font data is empty (which the same as IsNone in this
      case). }
    property IsEmpty: Boolean read GetIsNone;

    { Type of font-face that this data describes.

      It doesn't matter if the content is a single font or a collection. In
      any case the FaceType would always return the type of the font-face
      that will be created by IBLFontFace.CreateFromData. }
    property FaceType: TBLFontFaceType read GetFaceType;

    { The number of faces of this font-data.

      If the data is not initialized the result would be always zero. If the
      data is initialized to a single font it would be 1, and if the data is
      initialized to a font collection then the return would correspond to
      the number of font-faces within that collection.

      You should not use FaceCount to check whether the font is a collection as
      it's possible to have a font-collection with just a single font. Using
      IsCollection is more reliable and would always return the right value. }
    property FaceCount: Integer read GetFaceCount;

    { Font-data flags }
    property Flags: TBLFontDataFlags read GetFlags;

    { Whether this font-data is a font-collection. }
    property IsCollection: Boolean read GetIsCollection;

    { Internal handle for use with the C API }
    property Handle: PBLFontDataCore read GetHandle;
  end;

type
  { Implements IBLFontData }
  TBLFontData = class(TInterfacedObject, IBLFontData)
  {$REGION 'Internal Declarations'}
  private type
    TDestroyData = record
      FontData: IBLFontData;
      Event: TBLFontDataDestroyEvent;
    end;
    PDestroyData = ^TDestroyData;
  private
    FHandle: BLFontDataCore;
    FIsReference: Boolean;
  private
    class procedure DoDestroy(impl, destroyData: Pointer); cdecl; static;
  protected
    { IBLFontData }
    function GetIsNone: Boolean;
    function GetFaceType: TBLFontFaceType;
    function GetFaceCount: Integer;
    function GetFlags: TBLFontDataFlags;
    function GetIsCollection: Boolean;
    function GetHandle: PBLFontDataCore;

    procedure InitializeFromFile(const AFilename: String;
      const AReadFlags: TBLFileReadFlags = []);

    procedure InitializeFromData(const AData: TBytes); overload;
    procedure InitializeFromData(const AData: Pointer; const ASize: Integer;
      const AOnDestroy: TBLFontDataDestroyEvent = nil); overload;

    procedure Reset;
    function Equals(const AOther: IBLFontData): Boolean; reintroduce; overload;

    function GetTags(const AFaceIndex: Integer): TArray<TBLTag>;
    function QueryTable(const AFaceIndex: Integer; const ATag: TBLTag;
      out ATable: TBLFontTable): Integer;
    function QueryTables(const AFaceIndex: Integer; const ATags: TArray<TBLTag>;
      out ATables: TArray<TBLFontTable>): Integer;
  private
    constructor Create(const AHandle: BLFontDataCore;
      const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{ ============================================================================
   [BLFontFace]
  ============================================================================ }

type
  { Font face }
  IBLFontFace = interface
  ['{D51BEBAD-EB2A-41CA-B76C-5773E696CA80}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetWeight: TBLFontWeight;
    function GetStretch: TBLFontStretch;
    function GetStyle: TBLFontStyle;
    function GetFaceInfo: TBLFontFaceInfo;
    function GetFaceType: TBLFontFaceType;
    function GetOutlineType: TBLFontOutlineType;
    function GetFaceIndex: Integer;
    function GetFaceFlags: TBLFontFaceFlags;
    function GetHasTypographicNames: Boolean;
    function GetHasTypographicMetrics: Boolean;
    function GetHasCharToGlyphMapping: Boolean;
    function GetHasHorizontalMetrics: Boolean;
    function GetHasVerticalMetrics: Boolean;
    function GetHasHorizontalKerning: Boolean;
    function GetHasVerticalKerning: Boolean;
    function GetHasOpenTypeFeatures: Boolean;
    function GetHasPanoseData: Boolean;
    function GetHasUnicodeCoverage: Boolean;
    function GetHasBaselineYAt0: Boolean;
    function GetHasLSBPointXAt0: Boolean;
    function GetHasVariationSequences: Boolean;
    function GetHasOpenTypeVariations: Boolean;
    function GetIsSymbolFont: Boolean;
    function GetIsLastResortFont: Boolean;
    function GetDiagFlags: TBLFontFaceDiagFlags;
    function GetUniqueId: TBLUniqueId;
    function GetData: IBLFontData;
    function GetFullName: String;
    function GetFamilyName: String;
    function GetSubfamilyName: String;
    function GetPostScriptName: String;
    function GetDesignMetrics: TBLFontDesignMetrics;
    function GetUnitsPerEm: Integer;
    function GetPanose: TBLFontPanose;
    function GetUnicodeCoverage: TBLFontUnicodeCoverage;
    function GetHandle: PBLFontFaceCore;
    {$ENDREGION 'Internal Declarations'}

    { Initializes an IBLFontFace from a file specified by AFileName.

      This is a utility function that first creates a IBLFontData and then
      calls InitializeFromData(FontData, 0). See IBLFontData.CreateFromFile
      for more details, especially the use of AReadFlags is important for
      system fonts.

      This function offers a simplified creation of IBLFontFace directly
      from a file, but doesn't provide as much flexibility as InitializeFromData
      as it allows to specify a AFaceIndex, which can be used to load multiple
      font-faces from a TrueType/OpenType collection. The use of
      InitializeFromData is recommended for any serious font handling. }
    procedure InitializeFromFile(const AFilename: String;
      const AReadFlags: TBLFileReadFlags = []);

    { Initializes an IBLFontFace from IBLFontData at the given AFaceIndex. }
    procedure InitializeFromData(const AFontData: IBLFontData;
      const AFaceIndex: Integer);

    procedure Reset;
    function Equals(const AOther: IBLFontFace): Boolean;

    { Tests whether the font-face has a given AFlag set. }
    function HasFaceFlag(const AFlag: TBLFontFaceFlag): Boolean;

    { Whether the font face is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the font face is empty (which the same as IsNone in this
      case). }
    property IsEmpty: Boolean read GetIsNone;

    { Font weight (returns default weight in case this is a variable font). }
    property Weight: TBLFontWeight read GetWeight;

    { Font stretch (returns default weight in case this is a variable font). }
    property Stretch: TBLFontStretch read GetStretch;

    { Font style }
    property Style: TBLFontStyle read GetStyle;

    { Font-face information }
    property FaceInfo: TBLFontFaceInfo read GetFaceInfo;

    { Font-face type }
    property FaceType: TBLFontFaceType read GetFaceType;

    { Font-face outline type }
    property OutlineType: TBLFontOutlineType read GetOutlineType;

    { Zero-based index of this font-face.

      Face index does only make sense if this face is part of a TrueType
      or OpenType font collection. In that case the returned value would be
      the index of this face in that collection. If the face is not part of a
      collection then the returned value would always be zero. }
    property FaceIndex: Integer read GetFaceIndex;

    { Font-face flags }
    property FaceFlags: TBLFontFaceFlags read GetFaceFlags;

    { Tests whether the font-face uses typographic family and subfamily names. }
    property HasTypographicNames: Boolean read GetHasTypographicNames;

    { Tests whether the font-face uses typographic metrics. }
    property HasTypographicMetrics: Boolean read GetHasTypographicMetrics;

    { Tests whether the font-face provides character to glyph mapping. }
    property HasCharToGlyphMapping: Boolean read GetHasCharToGlyphMapping;

    { Tests whether the font-face has horizontal glyph metrics (advances, side
      bearings). }
    property HasHorizontalMetrics: Boolean read GetHasHorizontalMetrics;

    { Tests whether the font-face has vertical glyph metrics (advances, side
      bearings). }
    property HasVerticalMetrics: Boolean read GetHasVerticalMetrics;

    { Tests whether the font-face has a legacy horizontal kerning feature
      ('kern' table with horizontal kerning data). }
    property HasHorizontalKerning: Boolean read GetHasHorizontalKerning;

    { Tests whether the font-face has a legacy vertical kerning feature ('kern'
      table with vertical kerning data). }
    property HasVerticalKerning: Boolean read GetHasVerticalKerning;

    { Tests whether the font-face has OpenType features (GDEF, GPOS, GSUB). }
    property HasOpenTypeFeatures: Boolean read GetHasOpenTypeFeatures;

    { Tests whether the font-face has panose classification. }
    property HasPanoseData: Boolean read GetHasPanoseData;

    { Tests whether the font-face has unicode coverage information. }
    property HasUnicodeCoverage: Boolean read GetHasUnicodeCoverage;

    { Tests whether the font-face's baseline equals 0. }
    property HasBaselineYAt0: Boolean read GetHasBaselineYAt0;

    { Tests whether the font-face's left sidebearing point at `x` equals 0. }
    property HasLSBPointXAt0: Boolean read GetHasLSBPointXAt0;

    { Tests whether the font-face has unicode variation sequences feature. }
    property HasVariationSequences: Boolean read GetHasVariationSequences;

    { Tests whether the font-face has OpenType Font Variations feature. }
    property HasOpenTypeVariations: Boolean read GetHasOpenTypeVariations;

    { This is a symbol font. }
    property IsSymbolFont: Boolean read GetIsSymbolFont;

    { This is a last resort font. }
    property IsLastResortFont: Boolean read GetIsLastResortFont;

    { Font-face diagnostics flags }
    property DiagFlags: TBLFontFaceDiagFlags read GetDiagFlags;

    { A unique identifier describing this IBLFontFace. }
    property UniqueId: TBLUniqueId read GetUniqueId;

    { IBLFontData associated with this font-face. }
    property Data: IBLFontData read GetData;

    { Font full name }
    property FullName: String read GetFullName;

    { Family name }
    property FamilyName: String read GetFamilyName;

    { Font subfamily name }
    property SubfamilyName: String read GetSubfamilyName;

    { Font PostScript name }
    property PostScriptName: String read GetPostScriptName;

    { Design metrics of this IBLFontFace. }
    property DesignMetrics: TBLFontDesignMetrics read GetDesignMetrics;

    { Units per em, which are part of font's design metrics. }
    property UnitsPerEm: Integer read GetUnitsPerEm;

    { PANOSE classification of this IBLFontFace. }
    property Panose: TBLFontPanose read GetPanose;

    { Unicode coverage of this IBLFontFace.

      The returned unicode-coverage is not calculated by Blend2D so in general
      the value doesn't have to be correct. Use GetCharacterCoverage to get
      a coverage calculated by Blend2D at character granularity. }
    property UnicodeCoverage: TBLFontUnicodeCoverage read GetUnicodeCoverage;

    { Internal handle for use with the C API }
    property Handle: PBLFontFaceCore read GetHandle;
  end;

type
  { Implements IBLFontFace }
  TBLFontFace = class(TInterfacedObject, IBLFontFace)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontFaceCore;
    FData: IBLFontData;
    FIsReference: Boolean;
  protected
    { IBLFontFace }
    function GetIsNone: Boolean;
    function GetWeight: TBLFontWeight;
    function GetStretch: TBLFontStretch;
    function GetStyle: TBLFontStyle;
    function GetFaceInfo: TBLFontFaceInfo;
    function GetFaceType: TBLFontFaceType;
    function GetOutlineType: TBLFontOutlineType;
    function GetFaceIndex: Integer;
    function GetFaceFlags: TBLFontFaceFlags;
    function GetHasTypographicNames: Boolean;
    function GetHasTypographicMetrics: Boolean;
    function GetHasCharToGlyphMapping: Boolean;
    function GetHasHorizontalMetrics: Boolean;
    function GetHasVerticalMetrics: Boolean;
    function GetHasHorizontalKerning: Boolean;
    function GetHasVerticalKerning: Boolean;
    function GetHasOpenTypeFeatures: Boolean;
    function GetHasPanoseData: Boolean;
    function GetHasUnicodeCoverage: Boolean;
    function GetHasBaselineYAt0: Boolean;
    function GetHasLSBPointXAt0: Boolean;
    function GetHasVariationSequences: Boolean;
    function GetHasOpenTypeVariations: Boolean;
    function GetIsSymbolFont: Boolean;
    function GetIsLastResortFont: Boolean;
    function GetDiagFlags: TBLFontFaceDiagFlags;
    function GetUniqueId: TBLUniqueId;
    function GetData: IBLFontData;
    function GetFullName: String;
    function GetFamilyName: String;
    function GetSubfamilyName: String;
    function GetPostScriptName: String;
    function GetDesignMetrics: TBLFontDesignMetrics;
    function GetUnitsPerEm: Integer;
    function GetPanose: TBLFontPanose;
    function GetUnicodeCoverage: TBLFontUnicodeCoverage;
    function GetHandle: PBLFontFaceCore;

    procedure InitializeFromFile(const AFilename: String;
      const AReadFlags: TBLFileReadFlags = []);

    procedure InitializeFromData(const AFontData: IBLFontData;
      const AFaceIndex: Integer);

    procedure Reset;
    function Equals(const AOther: IBLFontFace): Boolean; reintroduce; overload;
    function HasFaceFlag(const AFlag: TBLFontFaceFlag): Boolean;
  private
    constructor Create(const AHandle: BLFontFaceCore;
      const AIsReference: Boolean); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create; overload;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{ ============================================================================
   [BLFont]
  ============================================================================ }

type
  { Font }
  IBLFont = interface
  ['{47ED75E4-8E6C-4D0E-95D9-6321CD4F8694}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetFaceType: TBLFontFaceType;
    function GetFaceFlags: TBLFontFaceFlags;
    function GetSize: Single;
    function GetUnitsPerEm: Integer;
    function GetFace: IBLFontFace;
    function GetFeatures: TArray<TBLFontFeature>;
    function GetVariations: TArray<TBLFontVariation>;
    function GetWeight: TBLFontWeight;
    function GetStretch: TBLFontStretch;
    function GetStyle: TBLFontStyle;
    function GetMatrix: TBLFontMatrix;
    function GetMetrics: TBLFontMetrics;
    function GetMetricsPtr: PBLFontMetrics;
    function GetDesignMetrics: TBLFontDesignMetrics;
    function GetHandle: PBLFontCore;
    {$ENDREGION 'Internal Declarations'}

    procedure InitializeFromFace(const AFace: IBLFontFace;
      const ASize: Single);

    procedure Reset;
    function Equals(const AOther: IBLFont): Boolean;

    procedure Shape(const AGlyphBuffer: IBLGlyphBuffer);
    procedure MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer); overload;
    procedure MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
      out AState: TBLGlyphMappingState); overload;

    { TODO : Check when Blend2D updates APositioningFlags }
    procedure PositionGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
      const APositioningFlags: Cardinal = $FFFFFFFF);

    procedure ApplyKerning(const AGlyphBuffer: IBLGlyphBuffer);
    procedure ApplyGSub(const AGlyphBuffer: IBLGlyphBuffer;
      const AIndex: Integer; const ALookups: TBLBitWord);
    procedure ApplyGPos(const AGlyphBuffer: IBLGlyphBuffer;
      const AIndex: Integer; const ALookups: TBLBitWord);
    function GetTextMetrics(const AGlyphBuffer: IBLGlyphBuffer): TBLTextMetrics;

    function GetGlyphBounds(const AGlyphData: PCardinal;
      const AGlyphAdvance, ACount: Integer): TArray<TBLBoxI>;
    function GetGlyphAdvances(const AGlyphData: PCardinal;
      const AGlyphAdvance, ACount: Integer): TArray<TBLGlyphPlacement>;
    function GetGlyphOutlines(const AGlyphId: Cardinal;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphOutlines(const AGlyphId: Cardinal;
      const AUserMatrix: TBLMatrix2D;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
      const AUserMatrix: TBLMatrix2D;
      const ASink: TBLPathSinkEvent): IBLPath; overload;

    { Whether the font is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    { Tests whether the font is empty (which the same as IsNone in this case). }
    property IsEmpty: Boolean read GetIsNone;

    { Type of the font's associated font-face }
    property FaceType: TBLFontFaceType read GetFaceType;

    { Flags of the font }
    property FaceFlags: TBLFontFaceFlags read GetFaceFlags;

    { Size of the font }
    property Size: Single read GetSize;

    { The "units per em" (UPEM) of the font's associated font-face. }
    property UnitsPerEm: Integer read GetUnitsPerEm;

    { The font's associated font-face.
      Returns the same font-face, which was passed to InitializeFromFace. }
    property Face: IBLFontFace read GetFace;

    { The features associated with the font. }
    property Features: TArray<TBLFontFeature> read GetFeatures;

    { The variations associated with the font. }
    property Variations: TArray<TBLFontVariation> read GetVariations;

    { The weight of the font. }
    property Weight: TBLFontWeight read GetWeight;

    { The stretch of the font. }
    property Stretch: TBLFontStretch read GetStretch;

    { The style of the font. }
    property Style: TBLFontStyle read GetStyle;

    { The 2x2 matrix of the font.

      The returned TBLFontMatrix is used to scale fonts from design units
      into user units. The matrix usually has a negative M11 member as
      fonts use a different coordinate system than Blend2D. }
    property Matrix: TBLFontMatrix read GetMatrix;

    { The scaled metrics of the font.

      The returned metrics is a scale of design metrics that match the font size
      and its options. }
    property Metrics: TBLFontMetrics read GetMetrics;
    property MetricsPtr: PBLFontMetrics read GetMetricsPtr;

    { The design metrics of the font.

      The returned metrics is compatible with the metrics of IBLFontFace
      associated with this font. }
    property DesignMetrics: TBLFontDesignMetrics read GetDesignMetrics;

    { Internal handle for use with the C API }
    property Handle: PBLFontCore read GetHandle;
  end;

type
  { Implements IBLFont }
  TBLFont = class(TInterfacedObject, IBLFont)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontCore;
    FFace: IBLFontFace;
    FSink: TBLPathSinkEvent;
    FSinkPath: IBLPath;
  private
    class function DoSink(path: PBLPathCore; info: Pointer;
      closure: Pointer): Cardinal; cdecl; static;
  protected
    { IBLFont }
    function GetIsNone: Boolean;
    function GetFaceType: TBLFontFaceType;
    function GetFaceFlags: TBLFontFaceFlags;
    function GetSize: Single;
    function GetUnitsPerEm: Integer;
    function GetFace: IBLFontFace;
    function GetFeatures: TArray<TBLFontFeature>;
    function GetVariations: TArray<TBLFontVariation>;
    function GetWeight: TBLFontWeight;
    function GetStretch: TBLFontStretch;
    function GetStyle: TBLFontStyle;
    function GetMatrix: TBLFontMatrix;
    function GetMetrics: TBLFontMetrics;
    function GetMetricsPtr: PBLFontMetrics;
    function GetDesignMetrics: TBLFontDesignMetrics;
    function GetHandle: PBLFontCore;

    procedure InitializeFromFace(const AFace: IBLFontFace;
      const ASize: Single);

    procedure Reset;
    function Equals(const AOther: IBLFont): Boolean; reintroduce; overload;

    procedure Shape(const AGlyphBuffer: IBLGlyphBuffer);
    procedure MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer); overload;
    procedure MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
      out AState: TBLGlyphMappingState); overload;

    procedure PositionGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
      const APositioningFlags: Cardinal);

    procedure ApplyKerning(const AGlyphBuffer: IBLGlyphBuffer);
    procedure ApplyGSub(const AGlyphBuffer: IBLGlyphBuffer;
      const AIndex: Integer; const ALookups: TBLBitWord);
    procedure ApplyGPos(const AGlyphBuffer: IBLGlyphBuffer;
      const AIndex: Integer; const ALookups: TBLBitWord);
    function GetTextMetrics(const AGlyphBuffer: IBLGlyphBuffer): TBLTextMetrics;

    function GetGlyphBounds(const AGlyphData: PCardinal;
      const AGlyphAdvance, ACount: Integer): TArray<TBLBoxI>;
    function GetGlyphAdvances(const AGlyphData: PCardinal;
      const AGlyphAdvance, ACount: Integer): TArray<TBLGlyphPlacement>;
    function GetGlyphOutlines(const AGlyphId: Cardinal;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphOutlines(const AGlyphId: Cardinal;
      const AUserMatrix: TBLMatrix2D;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
    function GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
      const AUserMatrix: TBLMatrix2D;
      const ASink: TBLPathSinkEvent): IBLPath; overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{$ENDREGION 'Font'}

{$REGION 'Font Manager'}

{ ============================================================================
   [BLFontManager]
  ============================================================================ }

type
  { Font data }
  IBLFontManager = interface
  ['{A75AAED4-30E3-4A65-978F-590528AB2DCF}']
    {$REGION 'Internal Declarations'}
    function GetIsNone: Boolean;
    function GetFaceCount: Integer;
    function GetFamilyCount: Integer;
    function GetHandle: PBLFontManagerCore;
    {$ENDREGION 'Internal Declarations'}

    procedure Reset;
    function Equals(const AOther: IBLFontManager): Boolean;

    procedure Initialize;

    { Whether the font manager contains the given font AFace. }
    function HasFace(const AFace: IBLFontFace): Boolean;

    { Adds a font AFace to the font manager. Does nothing if the manager already
      contans the font face.

      Important conditions:
      * TBLResultCode.FontNotInitializes is raised if the font AFace is invalid.
      * TBLResultCode.OutOfMemory is raised if memory allocation failed. }
    procedure AddFace(const AFace: IBLFontFace);

    { Queries a font face by family name and returns the font face or nil if
      not found. }
    function QueryFace(const AName: String): IBLFontFace; overload;

    { Queries a font face by family name and returns the font face or nil if
      not found.

      The AProperties parameter contains query properties that the query engine
      will consider when doing the match. The best candidate will be selected
      based on the following rules:
      * Style has the highest priority.
      * Weight has the lowest priority. }
    function QueryFace(const AName: String;
      const AProperties: TBLFontQueryProperties): IBLFontFace; overload;

    { Queries all font-faces by family name and returns an array of font faces. }
    function QueryFacesByFamilyName(const AName: String): TArray<IBLFontFace>;

    { Whether the font manager is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    { The number of IBLFontFace instances the font manager holds. }
    property FaceCount: Integer read GetFaceCount;

    { The number of unique font families the font manager holds. }
    property FamilyCount: Integer read GetFamilyCount;

    { Internal handle for use with the C API }
    property Handle: PBLFontManagerCore read GetHandle;
  end;

type
  { Implements IBLFontManager }
  TBLFontManager = class(TInterfacedObject, IBLFontManager)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLFontManagerCore;
  protected
    { IBLFontManager }
    function GetIsNone: Boolean;
    function GetFaceCount: Integer;
    function GetFamilyCount: Integer;
    function GetHandle: PBLFontManagerCore;

    procedure Reset;
    function Equals(const AOther: IBLFontManager): Boolean; reintroduce; overload;
    procedure Initialize;
    function HasFace(const AFace: IBLFontFace): Boolean;
    procedure AddFace(const AFace: IBLFontFace);
    function QueryFace(const AName: String): IBLFontFace; overload;
    function QueryFace(const AName: String;
      const AProperties: TBLFontQueryProperties): IBLFontFace; overload;
    function QueryFacesByFamilyName(const AName: String): TArray<IBLFontFace>;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;

{$ENDREGION 'Font Manager'}

{$REGION 'Pixel Converter'}

{ ============================================================================
   [BLPixelConverterCreateFlags]
  ============================================================================ }

type
  { Flags used by IBLPixelConverter.Initialize. }
  TBLPixelConverterCreateFlag = (
    { Specifies that the source palette in TBLFormatInfo doesn't have to be
      copied by IBLPixelConverter. The caller must ensure that the palette
      would stay valid until the pixel converter is destroyed. }
    DontCopyPalette  = 0,

    { Specifies that the source palette in TBLFormatInfo is alterable and
      the pixel converter can modify it when preparing the conversion. The
      modification can be irreversible so only use this flag when you are sure
      that the palette passed to IBLPixelConverter.Initialize won't be needed
      outside of pixel conversion.

      The flag DontCopyPalette must be set as well, otherwise this flag would be
      ignored. }
    AlterablePalette = 1,

    { When there is no built-in conversion between the given pixel formats it's
      possible to use an intermediate format that is used during conversion. In
      such case the base pixel converter creates two more converters that are
      then used internally.

      This option disables such feature - creating a pixel converter would fail
      with TBLResultCode.NotImplemented error if direct conversion is not
      possible. }
    NoMultiStep      = 2);
  TBLPixelConverterCreateFlags = set of TBLPixelConverterCreateFlag;

{ ============================================================================
   [BLPixelConverter - Options]
  ============================================================================ }

type
  { Pixel conversion options. }
  TBLPixelConverterOptions = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPixelConverterOptions;
    function GetOrigin: TBLPointI; inline;
    procedure SetOrigin(const AValue: TBLPointI); inline;
    function GetGap: Integer; inline;
    procedure SetGap(const AValue: Integer); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property Origin: TBLPointI read GetOrigin write SetOrigin;
    property Gap: Integer read GetGap write SetGap;
  end;
  PBLPixelConverterOptions = ^TBLPixelConverterOptions;

{ ============================================================================
   [BLPixelConverter]
  ============================================================================ }

type
  { Provides an interface to convert pixels between various pixel formats. The
    primary purpose of this class is to allow efficient conversion between
    pixel formats used natively by Blend2D and pixel formats used elsewhere,
    for example image codecs or native framebuffers.

    Note: A default-initialized converter has a valid conversion function that
    would return TBLResultCode.NotInitialized if invoked. Use IsInitialized
    to test whether the pixel converter was properly initialized. }
  IBLPixelConverter = interface
  ['{A75AAED4-30E3-4A65-978F-590528AB2DCF}']
    {$REGION 'Internal Declarations'}
    function GetIsInitialized: Boolean;
    function GetHandle: PBLPixelConverterCore;
    {$ENDREGION 'Internal Declarations'}

    { Initializes a new pixel converter that will convert pixels described by
      ASrcInfo into pixels described by ADstInfo.

      Use ACreateFlags to further specify the parameters of the conversion.

      Note: Destination and source format information must be valid, otherwise
      a TBLResultCode.InvalidValue error is raised. }
    procedure Initialize(const ASrcInfo, ADstInfo: TBLFormatInfo;
      const ACreateFlags: TBLPixelConverterCreateFlags = []);

    procedure Reset;

    { Assigns the AOther pixel converter into this one. }
    procedure Assign(const AOther: IBLPixelConverter);

    { Converts a single span of pixels of AWidth. }
    procedure ConvertSpan(const ASrcData, ADstData: Pointer;
      const AWidth: Integer); overload;
    procedure ConvertSpan(const ASrcData, ADstData: Pointer;
      const AWidth: Integer; const AOptions: TBLPixelConverterOptions); overload;

    { Converts a rectangular area of pixels from source format to destination. }
    procedure ConvertRect(const ASrcData: Pointer; const ASrcStride: Integer;
      const ADstData: Pointer; const ADstStride, AWidth, AHeight: Integer); overload;
    procedure ConvertRect(const ASrcData: Pointer; const ASrcStride: Integer;
      const ADstData: Pointer; const ADstStride, AWidth, AHeight: Integer;
      const AOptions: TBLPixelConverterOptions); overload;

    { Returns True if the converter is initialized. }
    property IsInitialized: Boolean read GetIsInitialized;

    { Internal handle for use with the C API }
    property Handle: PBLPixelConverterCore read GetHandle;
  end;

type
  { Implements IBLPixelConverter }
  TBLPixelConverter = class(TInterfacedObject, IBLPixelConverter)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLPixelConverterCore;
  protected
    { IBLPixelConverter }
    function GetIsInitialized: Boolean;
    function GetHandle: PBLPixelConverterCore;

    procedure Initialize(const ASrcInfo, ADstInfo: TBLFormatInfo;
      const ACreateFlags: TBLPixelConverterCreateFlags = []);

    procedure Reset;

    procedure Assign(const AOther: IBLPixelConverter);

    procedure ConvertSpan(const ASrcData, ADstData: Pointer;
      const AWidth: Integer); overload;
    procedure ConvertSpan(const ASrcData, ADstData: Pointer;
      const AWidth: Integer; const AOptions: TBLPixelConverterOptions); overload;

    procedure ConvertRect(const ASrcData: Pointer; const ASrcStride: Integer;
      const ADstData: Pointer; const ADstStride, AWidth, AHeight: Integer); overload;
    procedure ConvertRect(const ASrcData: Pointer; const ASrcStride: Integer;
      const ADstData: Pointer; const ADstStride, AWidth, AHeight: Integer;
      const AOptions: TBLPixelConverterOptions); overload;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    { Creates a pixel converter appropriate for the current platform. This is
      useful for platforms where the display format does not match the internal
      Blend2D format. For example, on macOS, iOS and Android, the red and blue
      color channels need to be swapped. On Windows, Blend2D's internal format
      matches the display format and no conversion is needed.

      On Windows, it returns a converter that doesn't convert, but just copies
      the data. However, it is more efficient to not use a pixel converter at
      all on Windows.

      On other platforms, it creates a converter that converts
      from PRGB to PBGR. }
    class function CreatePlatformConverter: IBLPixelConverter; static;
  end;

{$ENDREGION 'Pixel Converter'}

{$REGION 'Style'}

{ ============================================================================
   [BLStyle - Enums]
  ============================================================================ }

type
  { Style type. }
  TBLStyleType = (
    { No style, nothing will be paint. }
    None     = BL_STYLE_TYPE_NONE,

    { Solid color style. }
    Solid    = BL_STYLE_TYPE_SOLID,

    { Pattern style. }
    Pattern  = BL_STYLE_TYPE_PATTERN,

    { Gradient style. }
    Gradient = BL_STYLE_TYPE_GRADIENT);

{$ENDREGION 'Style'}

{$REGION 'Context'}

{ ============================================================================
   [BLContext - Enums]
  ============================================================================ }

type
  { Rendering context type. }
  TBLContextType = (
    { No rendering context. }
    None   = BL_CONTEXT_TYPE_NONE,

    { Dummy rendering context. }
    Dummy  = BL_CONTEXT_TYPE_DUMMY,

    { Software-accelerated rendering context. }
    Raster = BL_CONTEXT_TYPE_RASTER);

type
  { Rendering context hint. }
  TBLContextHint = (
    { Rendering quality. }
    RenderingQuality = BL_CONTEXT_HINT_RENDERING_QUALITY,

    { Gradient quality. }
    GradientQuality  = BL_CONTEXT_HINT_GRADIENT_QUALITY,

    { Pattern quality. }
    PatternQuality   = BL_CONTEXT_HINT_PATTERN_QUALITY);

type
  { Describes a rendering operation type - fill or stroke.

    The rendering context allows to get and set fill & stroke options directly
    or via "style" functions that take the rendering operation type (OpType)
    and dispatch the call to the right function. }
  TBLContextOpType = (
    { Fill operation type. }
    Fill   = BL_CONTEXT_OP_TYPE_FILL,

    { Stroke operation type. }
    Stroke = BL_CONTEXT_OP_TYPE_STROKE);

type
  { Rendering context flush-flags, use with IBLContext.Flush. }
  TBLContextFlushFlag = (
    _Dummy = 0,

    { Flush the command queue and wait for its completion (will block). }
    Sync = 31);
  TBLContextFlushFlags = set of TBLContextFlushFlag;

type
  { Rendering context create-flags. }
  TBLContextCreateFlag = (
    { Fallbacks to a synchronous rendering in case that the rendering engine
      wasn't able to acquire threads. This flag only makes sense when the
      asynchronous mode was specified by having TBLContextCreateInfo.ThreadCount
      greater than 0. If the rendering context fails to acquire at least one
      thread it would fallback to synchronous mode with no worker threads.

      Note: If this flag is specified with TBLContextCreateInfo.ThreadCount = 1
      it means to immediately fallback to synchronous rendering. It's only
      practical to use this flag with 2 or more requested threads. }
    FallbackToSync      = 3,

    { If this flag is specified and asynchronous rendering is enabled then
      the context would create its own isolated thread-pool, which is useful
      for debugging purposes.

      Do not use this flag in production as rendering contexts with isolated
      thread-pool have to create and destroy all threads they use. This flag
      is only useful for testing, debugging, and isolated benchmarking. }
    IsolatedThreadPool  = 24,

    { If this flag is specified and JIT pipeline generation enabled then the
      rendering context would create its own isolated JIT runtime. which is
      useful for debugging purposes. This flag will be ignored if JIT pipeline
      generation is either not supported or was disabled by other flags.

      Do not use this flag in production as rendering contexts with isolated
      JIT runtime do not use global pipeline cache, that's it, after the
      rendering context is destroyed the JIT runtime is destroyed with it with
      all compiled pipelines. This flag is only useful for testing, debugging,
      and isolated benchmarking. }
    IsolatedJit         = 25,

    { Override CPU features when creating isolated context. }
    OverrideCpuFeatures = 26);
  TBLContextCreateFlags = set of TBLContextCreateFlag;

type
  { Specifies a rendering context property that can be specific to the rendering
    context implementation and that doesn't have its own C and C++ API.
    Different rendering context implementations may expose various properties
    that users can query to get more details about the rendering context itself,
    rendering details (like optimizations or possibly limitations), memory
    details, and other information that was collected during the rendering.

    Properties are never part of the rendering context state - they are
    stateless and are not subject to Save and Restore. Many properties are
    purely informative, but some not, e.g. AccumulatedErrorFlags. }
  TBLContextProperty = (
    { Number of threads that the rendering context uses for rendering. }
    ThreadCount           = BL_CONTEXT_PROPERTY_THREAD_COUNT,

    { Accumulated errors collected during the lifetime of the rendering
      context. }
    AccumulatedErrorFlags = BL_CONTEXT_PROPERTY_ACCUMULATED_ERROR_FLAGS);

type
  { Error flags that are accumulated during the rendering context lifetime and
    that can be queried through TBLContext.QueryAccumulatedErrorFlags. The
    reason why these flags exist is that errors can happen during asynchronous
    rendering, and there is no way the user can catch these errors. }
  TBLContextErrorFlag = (
    { The rendering context returned or encountered TBLResultCode.InvalidValue,
      which is mostly related to function argument handling. It's very likely
      some argument was wrong when calling TBLContext API. }
    InvalidValue        = 0,

    { Invalid state describes something wrong, for example pipeline compilation
      problem. }
    InvalidState        = 1,

    { The rendering context has encountered invalid geometry. }
    InvalidGeometry     = 2,

    { The rendering context has encountered invalid glyph. }
    InvalidGlyph        = 3,

    { The rendering context has encountered invalid or uninitialized font. }
    InvalidFont         = 4,

    { Thread pool was exhausted and couldn't acquire the requested number of
      threads. }
    ThreadPoolExhausted = 29,

    { Out of memory condition. }
    OutOfMemory         = 30,

    { Unknown error, which we don't have flag for. }
    UnknownError        = 31);
  TBLContextErrorFlags = set of TBLContextErrorFlag;

type
  { Clip mode. }
  TBLClipMode = (
    { Clipping to a rectangle that is aligned to the pixel grid. }
    AlignedRect   = BL_CLIP_MODE_ALIGNED_RECT,

    { Clipping to a rectangle that is not aligned to pixel grid. }
    UnalignedRect = BL_CLIP_MODE_UNALIGNED_RECT,

    { Clipping to a non-rectangular area that is defined by using mask. }
    Mask          = BL_CLIP_MODE_MASK);

type
  { Composition & blending operator. }
  TBLCompOp = (
    { Source-over [default]. }
    SrcOver     = BL_COMP_OP_SRC_OVER,

    { Source-copy. }
    SrcCopy     = BL_COMP_OP_SRC_COPY,

    { Source-in. }
    SrcIn       = BL_COMP_OP_SRC_IN,

    { Source-out. }
    SrcOut      = BL_COMP_OP_SRC_OUT,

    { Source-atop. }
    SrcAtop     = BL_COMP_OP_SRC_ATOP,

    { Destination-over. }
    DstOver     = BL_COMP_OP_DST_OVER,

    { Destination-copy [nop]. }
    DstCopy     = BL_COMP_OP_DST_COPY,

    { Destination-in. }
    DstIn       = BL_COMP_OP_DST_IN,

    { Destination-out. }
    DstOut      = BL_COMP_OP_DST_OUT,

    { Destination-atop. }
    DstAtop     = BL_COMP_OP_DST_ATOP,

    { Xor. }
    ExclusiveOr = BL_COMP_OP_XOR,

    { Clear. }
    Clear       = BL_COMP_OP_CLEAR,

    { Plus. }
    Plus        = BL_COMP_OP_PLUS,

    { Minus. }
    Minus       = BL_COMP_OP_MINUS,

    { Modulate. }
    Modulate    = BL_COMP_OP_MODULATE,

    { Multiply. }
    Multiply    = BL_COMP_OP_MULTIPLY,

    { Screen. }
    Screen      = BL_COMP_OP_SCREEN,

    { Overlay. }
    Overlay     = BL_COMP_OP_OVERLAY,

    { Darken. }
    Darken      = BL_COMP_OP_DARKEN,

    { Lighten. }
    Lighten     = BL_COMP_OP_LIGHTEN,

    { Color dodge. }
    ColorDodge  = BL_COMP_OP_COLOR_DODGE,

    { Color burn. }
    ColorBurn   = BL_COMP_OP_COLOR_BURN,

    { Linear burn. }
    LinearBurn  = BL_COMP_OP_LINEAR_BURN,

    { Linear light. }
    LinearLight = BL_COMP_OP_LINEAR_LIGHT,

    { Pin light. }
    PinLight    = BL_COMP_OP_PIN_LIGHT,

    { Hard-light. }
    HardLight   = BL_COMP_OP_HARD_LIGHT,

    { Soft-light. }
    SoftLight   = BL_COMP_OP_SOFT_LIGHT,

    { Difference. }
    Difference  = BL_COMP_OP_DIFFERENCE,

    { Exclusion. }
    Exclusion   = BL_COMP_OP_EXCLUSION);

type
  { Gradient rendering quality. }
  TBLGradientQuality = (
    { Nearest neighbor. }
    Nearest = BL_GRADIENT_QUALITY_NEAREST);

type
  { Pattern quality. }
  TBLPatternQuality = (
    { Nearest neighbor. }
    Nearest  = BL_PATTERN_QUALITY_NEAREST,

    { Bilinear. }
    Bilinear = BL_PATTERN_QUALITY_BILINEAR);

type
  { Rendering quality. }
  TBLRenderingQuality = (
    { Render using anti-aliasing. }
    AntiAlias = BL_RENDERING_QUALITY_ANTIALIAS);

{ ============================================================================
   [BLContext - CreateInfo]
  ============================================================================ }

type
  { Information that can be used to customize the rendering context. }
  TBLContextCreateInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLContextCreateInfo;
    function GetFlags: TBLContextCreateFlags; inline;
    procedure SetFlags(const AValue: TBLContextCreateFlags); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; inline;

    { Create flags, see TBLContextCreateFlags. }
    property Flags: TBLContextCreateFlags read GetFlags write SetFlags;

    { Number of worker threads to use for asynchronous rendering, if non-zero.

      If ThreadCount is zero it means to initialize the context for synchronous
      rendering. This means that every operation will take effect immediately.
      If ThreadCount is 1 it means that the rendering will be asynchronous, but
      no thread would be acquired from a thread-pool, because the user thread
      will be used as a worker. And finally, if ThreadCount is greater than 1
      then total of ThreadCount - 1 threads will be acquired from thread-pool
      and used as additional workers. }
    property ThreadCount: Integer read FHandle.threadCount write FHandle.threadCount;

    { CPU features to use in isolated JIT runtime (if supported), only used
      when Flags contains TBLContextCreateFlag.OverrideCpuFeatures. }
    property CpuFeatures: Cardinal read FHandle.cpuFeatures write FHandle.cpuFeatures;

    { Maximum number of commands to be queued.

      If this parameter is zero the queue size will be determined automatically.

      TODO: To be documented, has no effect at the moment. }
    property CommandQueueLimit: Integer read FHandle.commandQueueLimit write FHandle.commandQueueLimit;
  end;
  PBLContextCreateInfo = ^TBLContextCreateInfo;

{ ============================================================================
   [BLContext - Cookie]
  ============================================================================ }

type
  { Holds an arbitrary 128-bit value (cookie) that can be used to match other
    cookies. Blend2D uses cookies in places where it allows to "lock" some
    state that can only be unlocked by a matching cookie. Please don't confuse
    cookies with a security of any kind, it's just an arbitrary data that must
    match to proceed with a certain operation.

    Cookies can be used with IBLContext.Save and IBLContextRestore operations. }
  TBLContextCookie = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLContextCookie;
    function GetIsEmpty: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLContextCookie): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLContextCookie): Boolean; inline; static;
  public
    procedure Reset; overload; inline;
    procedure Reset(const AOther: TBLContextCookie); overload; inline;
    procedure Reset(const AData0, AData1: UInt64); overload; inline;

    function Equals(const AOther: TBLContextCookie): Boolean; inline;

    property IsEmpty: Boolean read GetIsEmpty;
  end;
  PBLContextCookie = ^TBLContextCookie;

{ ============================================================================
   [BLContext - Hints]
  ============================================================================ }

type
  { Rendering context hints. }
  TBLContextHints = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLContextHints;
    function GetGradientQuality: TBLGradientQuality; inline;
    function GetPatternQuality: TBLPatternQuality; inline;
    function GetRenderingQuality: TBLRenderingQuality; inline;
    procedure SetGradientQuality(const AValue: TBLGradientQuality); inline;
    procedure SetPatternQuality(const AValue: TBLPatternQuality); inline;
    procedure SetRenderingQuality(const AValue: TBLRenderingQuality); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    property RenderingQuality: TBLRenderingQuality read GetRenderingQuality write SetRenderingQuality;
    property GradientQuality: TBLGradientQuality read GetGradientQuality write SetGradientQuality;
    property PatternQuality: TBLPatternQuality read GetPatternQuality write SetPatternQuality;
  end;
  PBLContextHints = ^TBLContextHints;

{ ============================================================================
   [BLContext - State]
  ============================================================================ }

type
  { Rendering context state.

    This state is not meant to be created by users, it's only provided for users
    that want to introspect the rendering context state. }
  TBLContextState = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLContextState;
    function GetApproximationOptions: TBLApproximationOptions; inline;
    function GetCompOp: TBLCompOp; inline;
    function GetFillRule: TBLFillRule; inline;
    function GetFillStyle: TBLStyleType; overload; inline;
    function GetHints: TBLContextHints; inline;
    function GetMetaMatrix: TBLMatrix2D; inline;
    function GetSavedStateCount: Integer; inline;
    function GetStrokeOptions: TBLStrokeOptions; inline;
    function GetStrokeStyle: TBLStyleType; overload; inline;
    function GetTargetImage: IBLImage; inline;
    function GetTargetSize: TBLSize; inline;
    function GetUserMatrix: TBLMatrix2D; inline;
    function GetFillAlpha: Double; inline;
    function GetStrokeAlpha: Double; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Target image or image object with nil impl in case that the rendering
      context doesn't render to an image. }
    property TargetImage: IBLImage read GetTargetImage;

    { Current size of the target in abstract units, pixels if rendering to
      IBLImage. }
    property TargetSize: TBLSize read GetTargetSize;

    { Current context hints. }
    property Hints: TBLContextHints read GetHints;

    { Current composition operator. }
    property CompOp: TBLCompOp read GetCompOp;

    { Current fill rule. }
    property FillRule: TBLFillRule read GetFillRule;

    { Current type of a style for fill operations. }
    property FillStyle: TBLStyleType read GetFillStyle;

    { Current type of a style for stroke operations. }
    property StrokeStyle: TBLStyleType read GetStrokeStyle;

    { Approximation options. }
    property ApproximationOptions: TBLApproximationOptions read GetApproximationOptions;

    { Current global alpha value [0, 1]. }
    property GlobalAlpha: Double read FHandle.globalAlpha;

    { Current fill alpha }
    property FillAlpha: Double read GetFillAlpha;

    { Current stroke alpha }
    property StrokeAlpha: Double read GetStrokeAlpha;

    { Current stroke options. }
    property StrokeOptions: TBLStrokeOptions read GetStrokeOptions;

    { Current meta transformation matrix. }
    property MetaMatrix: TBLMatrix2D read GetMetaMatrix;

    { Current user transformation matrix. }
    property UserMatrix: TBLMatrix2D read GetUserMatrix;

    { Count of saved states in the context. }
    property SavedStateCount: Integer read GetSavedStateCount;
  end;
  PBLContextState = ^TBLContextState;

{ ============================================================================
   [BLContext]
  ============================================================================ }

type
  { Rendering context }
  IBLContext = interface
  ['{079F0FF5-12D6-4D53-B5AA-79C1AEE83068}']
    {$REGION 'Internal Declarations'}
    function GetTargetSize: TBLSize;
    function GetTargetWidth: Double;
    function GetTargetHeight: Double;
    function GetTargetImage: IBLImage;
    function GetContextType: TBLContextType;
    function GetIsNone: Boolean;
    function GetSavedStateCount: Integer;
    function GetMetaMatrix: TBLMatrix2D;
    function GetUserMatrix: TBLMatrix2D;
    procedure SetUserMatrix(const AValue: TBLMatrix2D);
    function GetHints: TBLContextHints;
    procedure SetHints(const AValue: TBLContextHints);
    function GetRenderingQuality: TBLRenderingQuality;
    procedure SetRenderingQuality(const AValue: TBLRenderingQuality);
    function GetGradientQuality: TBLGradientQuality;
    procedure SetGradientQuality(const AValue: TBLGradientQuality);
    function GetPatternQuality: TBLPatternQuality;
    procedure SetPatternQuality(const AValue: TBLPatternQuality);
    function GetApproximationOptions: TBLApproximationOptions;
    function GetFlattenMode: TBLFlattenMode;
    procedure SetFlattenMode(const AValue: TBLFlattenMode);
    function GetFlattenTolerance: Double;
    procedure SetFlattenTolerance(const AValue: Double);
    function GetCompOp: TBLCompOp;
    procedure SetCompOp(const AValue: TBLCompOp);
    function GetGlobalAlpha: Double;
    procedure SetGlobalAlpha(const AValue: Double);
    function GetFillStyle: TBLStyleType; overload;
    function GetStrokeStyle: TBLStyleType; overload;
    function GetFillAlpha: Double;
    procedure SetFillAlpha(const AValue: Double);
    function GetStrokeAlpha: Double;
    procedure SetStrokeAlpha(const AValue: Double);
    function GetFillColor: TBLRgba32;
    procedure SetFillColor(const AValue: TBLRgba32);
    function GetFillColor64: TBLRgba64;
    procedure SetFillColor64(const AValue: TBLRgba64);
    function GetFillColorF: TBLRgba;
    procedure SetFillColorF(const AValue: TBLRgba);
    function GetStrokeColor: TBLRgba32;
    procedure SetStrokeColor(const AValue: TBLRgba32);
    function GetStrokeColor64: TBLRgba64;
    procedure SetStrokeColor64(const AValue: TBLRgba64);
    function GetStrokeColorF: TBLRgba;
    procedure SetStrokeColorF(const AValue: TBLRgba);
    function GetFillPattern: IBLPattern;
    procedure SetFillPattern(const AValue: IBLPattern);
    function GetStrokePattern: IBLPattern;
    procedure SetStrokePattern(const AValue: IBLPattern);
    function GetFillGradient: IBLGradient;
    procedure SetFillGradient(const AValue: IBLGradient);
    function GetStrokeGradient: IBLGradient;
    procedure SetStrokeGradient(const AValue: IBLGradient);
    function GetFillRule: TBLFillRule;
    procedure SetFillRule(const AValue: TBLFillRule);
    function GetStrokeWidth: Double;
    procedure SetStrokeWidth(const AValue: Double);
    function GetStrokeMiterLimit: Double;
    procedure SetStrokeMiterLimit(const AValue: Double);
    function GetStrokeJoin: TBLStrokeJoin;
    procedure SetStrokeJoin(const AValue: TBLStrokeJoin);
    function GetStrokeStartCap: TBLStrokeCap;
    procedure SetStrokeStartCap(const AValue: TBLStrokeCap);
    function GetStrokeEndCap: TBLStrokeCap;
    procedure SetStrokeEndCap(const AValue: TBLStrokeCap);
    function GetStrokeDashOffset: Double;
    procedure SetStrokeDashOffset(const AValue: Double);
    function GetStrokeDashArray: TArray<Double>;
    procedure SetStrokeDashArray(const AValue: TArray<Double>);
    function GetStrokeTransformOrder: TBLStrokeTransformOrder;
    procedure SetStrokeTransformOrder(const AValue: TBLStrokeTransformOrder);
    function GetStrokeOptions: TBLStrokeOptions;
    procedure SetStrokeOptions(const AValue: TBLStrokeOptions);
    function GetHandle: PBLContextCore;
    {$ENDREGION 'Internal Declarations'}

    { Resets this rendering context to the default constructed one.

      Similar behavior to the destructor, but the context will still be valid
      after Reset and would behave like a default constructed context. }
    procedure Reset;

    { Whether this and AOther point to the same rendering context. }
    function Equals(const AOther: IBLContext): Boolean; overload;

    { Starts rendering to the given AImage.

      If this operation succeeds then the rendering context will have exclusive
      access to the image data. This means that no other renderer can use it
      during rendering. }
    procedure Start(const AImage: IBLImage); overload;
    procedure Start(const AImage: IBLImage;
      const ACreateInfo: TBLContextCreateInfo); overload;

    { Waits for completion of all render commands and detaches the rendering
      context from the rendering target. After Finish completes the rendering
      context implementation would be released and replaced by a built-in null
      instance (no context). }
    procedure Finish;

    { Flushes the context }
    procedure Flush(const AFlags: TBLContextFlushFlags);

    { Queries the number of threads that the rendering context uses.

      If the returned value is zero it means that the rendering is synchronous,
      otherwise it describes the number of threads used for asynchronous
      rendering which include the user thread. For example if the returned value
      is 2 it means that the rendering context uses the user thread and one more
      worker. }
    function QueryThreadCount: Integer;

    { Queries accumulated errors as flags.

      Errors may accumulate during the lifetime of the rendering context. }
    function QueryAccumulatedErrorFlags: TBLContextErrorFlags;

    { Saves the current rendering context state.

      Blend2D uses optimizations that make Save a cheap operation. Only core
      values are actually saved in Save, others will only be saved if they
      are modified. This means that consecutive calls to Save and Restore
      do almost nothing. }
    procedure Save; overload;

    { Saves the current rendering context state and creates a restoration
      ACookie.

      If you use a ACookie to save a state you have to use the same cookie to
      restore it otherwise the Restore would fail. Please note that cookies
      are not a means of security, they are provided for making it easier to
      guarantee that a code that you may not control won't break your context. }
    procedure Save(out ACookie: TBLContextCookie); overload;

    { Restores the top-most saved context-state.

      Possible errors:
      * TBLResultCode.NoStatesToRestore: There are no saved states to restore.
      * TBLResultCode.NoMatchingCookie: Previous state was saved with cookie,
        which was not provided. You would need the correct cookie to restore
        such state. }
    procedure Restore; overload;

    { Restores to the point that matches the given ACookie.

      More than one state can be restored in case that the ACookie points to
      some previous state in the list.

      Possible errors:
      * TBLResultCode.NoStatesToRestore: There are no saved states to restore.
      * TBLResultCode.NoMatchingCookie: The cookie did't match any saved state. }
    procedure Restore(const ACookie: TBLContextCookie); overload;

    { Resets user matrix to identity. }
    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const APoint: TBLPointI); overload;
    procedure Translate(const APoint: TBLPoint); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const APoint: TBLPointI); overload;
    procedure Scale(const APoint: TBLPoint); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const APoint: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const APoint: TBLPointI); overload;
    procedure Rotate(const AAngle: Double; const APoint: TBLPoint); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const APoint: TBLPointI); overload;
    procedure PostTranslate(const APoint: TBLPoint); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const APoint: TBLPointI); overload;
    procedure PostScale(const APoint: TBLPoint); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const APoint: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const APoint: TBLPointI); overload;
    procedure PostRotate(const AAngle: Double; const APoint: TBLPoint); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);

    { Store the result of combining the current MetaMatrix and UserMatrix
      to MetaMatrix and reset UserMatrix to identity.

      Please note that this operation is irreversible. The only way to restore
      both matrices to the state before the call to UserToMeta is to use
      Save and Restore functions. }
    procedure UserToMeta;

    { These are alternatives to the FillColor, FillColor64, FillPattern and
      FillGradient properties. }
    procedure GetFillStyle(out ARgba: TBLRgba32); overload;
    procedure GetFillStyle(out ARgba: TBLRgba64); overload;
    procedure GetFillStyle(out ARgba: TBLRgba); overload;
    procedure GetFillStyle(out APattern: IBLPattern); overload;
    procedure GetFillStyle(out AGradient: IBLGradient); overload;

    procedure SetFillStyle(const ARgba: TBLRgba32); overload;
    procedure SetFillStyle(const ARgba: TBLRgba64); overload;
    procedure SetFillStyle(const ARgba: TBLRgba); overload;
    procedure SetFillStyle(const APattern: IBLPattern); overload;
    procedure SetFillStyle(const AGradient: IBLGradient); overload;
    procedure SetFillStyle(const AImage: IBLImage); overload;

    { These are alternatives to the StrokeColor, StrokeColor64, StrokePattern
      and StrokeGradient properties. }
    procedure GetStrokeStyle(out ARgba: TBLRgba32); overload;
    procedure GetStrokeStyle(out ARgba: TBLRgba64); overload;
    procedure GetStrokeStyle(out ARgba: TBLRgba); overload;
    procedure GetStrokeStyle(out APattern: IBLPattern); overload;
    procedure GetStrokeStyle(out AGradient: IBLGradient); overload;

    procedure SetStrokeStyle(const ARgba: TBLRgba32); overload;
    procedure SetStrokeStyle(const ARgba: TBLRgba64); overload;
    procedure SetStrokeStyle(const ARgba: TBLRgba); overload;
    procedure SetStrokeStyle(const APattern: IBLPattern); overload;
    procedure SetStrokeStyle(const AGradient: IBLGradient); overload;
    procedure SetStrokeStyle(const AImage: IBLImage); overload;

    { Restores clipping to the last saved state or to the context default
      clipping if there is no saved state.

      If there are no saved states then it resets clipping completely to the
      initial state that was used when the rendering context was created. }
    procedure RestoreClipping;
    procedure ClipToRect(const ARect: TBLRectI); overload;
    procedure ClipToRect(const ARect: TBLRect); overload;
    procedure ClipToRect(const AX, AY, AW, AH: Double); overload;

    { Clear everything. }
    procedure ClearAll;

    { Clears a rectangle ARect. }
    procedure ClearRect(const ARect: TBLRectI); overload;
    procedure ClearRect(const ARect: TBLRect); overload;
    procedure ClearRect(const AX, AY, AW, AH: Double); overload;

    { Fills everything. }
    procedure FillAll;

    { Fills a box. }
    procedure FillBox(const ABox: TBLBoxI); overload;
    procedure FillBox(const ABox: TBLBox); overload;
    procedure FillBox(const AX0, AY0, AX1, AY1: Double); overload;

    { Fills a rectangle ARect. }
    procedure FillRect(const ARect: TBLRectI); overload;
    procedure FillRect(const ARect: TBLRect); overload;
    procedure FillRect(const AX, AY, AW, AH: Double); overload;

    { Fills a circle. }
    procedure FillCircle(const ACircle: TBLCircle); overload;
    procedure FillCircle(const ACX, ACY, AR: Double); overload;

    { Fills an ellipse. }
    procedure FillEllipse(const AEllipse: TBLEllipse); overload;
    procedure FillEllipse(const ACX, ACY, ARX, ARY: Double); overload;

    { Fills a rounded rectangle. }
    procedure FillRoundRect(const ARoundRect: TBLRoundRect); overload;
    procedure FillRoundRect(const ARect: TBLRect; const AR: Double); overload;
    procedure FillRoundRect(const ARect: TBLRect; const ARX, ARY: Double); overload;
    procedure FillRoundRect(const AX, AY, AW, AH, AR: Double); overload;
    procedure FillRoundRect(const AX, AY, AW, AH, ARX, ARY: Double); overload;

    { Fills a chord. }
    procedure FillChord(const AChord: TBLArc); overload;
    procedure FillChord(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure FillChord(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    { Fills a pie. }
    procedure FillPie(const APie: TBLArc); overload;
    procedure FillPie(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure FillPie(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    { Fills a triangle. }
    procedure FillTriangle(const ATriangle: TBLTriangle); overload;
    procedure FillTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload;

    { Fills a polygon. }
    procedure FillPolygon(const APoly: TArray<TBLPoint>); overload;
    procedure FillPolygon(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure FillPolygon(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure FillPolygon(const APoly: TArray<TBLPointI>); overload;
    procedure FillPolygon(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure FillPolygon(const APoly: PBLPointI; const ACount: Integer); overload;

    { Fills an array of boxes. }
    procedure FillBoxArray(const ABoxes: TArray<TBLBox>); overload;
    procedure FillBoxArray(const ABoxes: TBLArrayView<TBLBox>); overload;
    procedure FillBoxArray(const ABoxes: PBLBox; const ACount: Integer); overload;
    procedure FillBoxArray(const ABoxes: TArray<TBLBoxI>); overload;
    procedure FillBoxArray(const ABoxes: TBLArrayView<TBLBoxI>); overload;
    procedure FillBoxArray(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    { Fills an array of rectangles. }
    procedure FillRectArray(const ARects: TArray<TBLRect>); overload;
    procedure FillRectArray(const ARects: TBLArrayView<TBLRect>); overload;
    procedure FillRectArray(const ARects: PBLRect; const ACount: Integer); overload;
    procedure FillRectArray(const ARects: TArray<TBLRectI>); overload;
    procedure FillRectArray(const ARects: TBLArrayView<TBLRectI>); overload;
    procedure FillRectArray(const ARects: PBLRectI; const ACount: Integer); overload;

    { Fills the given ARegion. }
    procedure FillRegion(const ARegion: IBLRegion);

    { Fills the given Path. }
    procedure FillPath(const APath: IBLPath);

    { Fills the passed text by using the given AFont. }
    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: String); overload;
    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UCS4String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UCS4String); overload;

    { Fills the passed AGlyphRun by using the given AFont. }
    procedure FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: PBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: PBLGlyphRun); overload;

    { Strokes a box. }
    procedure StrokeBox(const ABox: TBLBoxI); overload;
    procedure StrokeBox(const ABox: TBLBox); overload;
    procedure StrokeBox(const AX0, AY0, AX1, AY1: Double); overload;

    { Strokes a rectangle. }
    procedure StrokeRect(const ARect: TBLRectI); overload;
    procedure StrokeRect(const ARect: TBLRect); overload;
    procedure StrokeRect(const AX, AY, AW, AH: Double); overload;

    { Strokes a line. }
    procedure StrokeLine(const ALine: TBLLine); overload;
    procedure StrokeLine(const AP0, AP1: TBLPoint); overload;
    procedure StrokeLine(const AX0, AY0, AX1, AY1: Double); overload;

    { Strokes a circle. }
    procedure StrokeCircle(const ACircle: TBLCircle); overload;
    procedure StrokeCircle(const ACX, ACY, AR: Double); overload;

    { Strokes an ellipse. }
    procedure StrokeEllipse(const AEllipse: TBLEllipse); overload;
    procedure StrokeEllipse(const ACX, ACY, ARX, ARY: Double); overload;

    { Strokes a rounded rectangle. }
    procedure StrokeRoundRect(const ARoundRect: TBLRoundRect); overload;
    procedure StrokeRoundRect(const ARect: TBLRect; const AR: Double); overload;
    procedure StrokeRoundRect(const ARect: TBLRect; const ARX, ARY: Double); overload;
    procedure StrokeRoundRect(const AX, AY, AW, AH, AR: Double); overload;
    procedure StrokeRoundRect(const AX, AY, AW, AH, ARX, ARY: Double); overload;

    { Strokes an arc. }
    procedure StrokeArc(const AArc: TBLArc); overload;
    procedure StrokeArc(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokeArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    { Strokes a chord. }
    procedure StrokeChord(const AChord: TBLArc); overload;
    procedure StrokeChord(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokeChord(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    { Strokes a pie. }
    procedure StrokePie(const APie: TBLArc); overload;
    procedure StrokePie(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokePie(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    { Strokes a triangle. }
    procedure StrokeTriangle(const ATriangle: TBLTriangle); overload;
    procedure StrokeTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload;

    { Strokes a polyline. }
    procedure StrokePolyline(const APoly: TArray<TBLPoint>); overload;
    procedure StrokePolyline(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure StrokePolyline(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure StrokePolyline(const APoly: TArray<TBLPointI>); overload;
    procedure StrokePolyline(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure StrokePolyline(const APoly: PBLPointI; const ACount: Integer); overload;

    { Strokes a polygon. }
    procedure StrokePolygon(const APoly: TArray<TBLPoint>); overload;
    procedure StrokePolygon(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure StrokePolygon(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure StrokePolygon(const APoly: TArray<TBLPointI>); overload;
    procedure StrokePolygon(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure StrokePolygon(const APoly: PBLPointI; const ACount: Integer); overload;

    { Strokes an array of boxes. }
    procedure StrokeBoxArray(const ABoxes: TArray<TBLBox>); overload;
    procedure StrokeBoxArray(const ABoxes: TBLArrayView<TBLBox>); overload;
    procedure StrokeBoxArray(const ABoxes: PBLBox; const ACount: Integer); overload;
    procedure StrokeBoxArray(const ABoxes: TArray<TBLBoxI>); overload;
    procedure StrokeBoxArray(const ABoxes: TBLArrayView<TBLBoxI>); overload;
    procedure StrokeBoxArray(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    { Strokes an array of rectangles. }
    procedure StrokeRectArray(const ARects: TArray<TBLRect>); overload;
    procedure StrokeRectArray(const ARects: TBLArrayView<TBLRect>); overload;
    procedure StrokeRectArray(const ARects: PBLRect; const ACount: Integer); overload;
    procedure StrokeRectArray(const ARects: TArray<TBLRectI>); overload;
    procedure StrokeRectArray(const ARects: TBLArrayView<TBLRectI>); overload;
    procedure StrokeRectArray(const ARects: PBLRectI; const ACount: Integer); overload;

    { Strokes the given Path. }
    procedure StrokePath(const APath: IBLPath);

    { Strokes the passed text by using the given AFont. }
    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: String); overload;
    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UCS4String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UCS4String); overload;

    { Strokes the passed AGlyphRun by using the given AFont. }
    procedure StrokeGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure StrokeGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;

    { Blits source image ASrc at coordinates specified by ADst. }
    procedure BlitImage(const ADst: TBLPoint; const ASrc: IBLImage); overload;
    procedure BlitImage(const ADst: TBLPointI; const ASrc: IBLImage); overload;

    { Blits an area of source image ASrc specified by ASrcArea at coordinates
      specified by ADst.}
    procedure BlitImage(const ADst: TBLPoint; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;
    procedure BlitImage(const ADst: TBLPointI; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;

    { Blits a source image ASrc scaled to fit into ADst rectangle. }
    procedure BlitImage(const ADst: TBLRect; const ASrc: IBLImage); overload;
    procedure BlitImage(const ADst: TBLRectI; const ASrc: IBLImage); overload;

    { Blits an area of source image ASrc specified by ASrcArea scaled to fit
      into ADst rectangle. }
    procedure BlitImage(const ADst: TBLRect; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;
    procedure BlitImage(const ADst: TBLRectI; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;

    { Target size in abstract units (pixels in case of IBLImage) }
    property TargetSize: TBLSize read GetTargetSize;

    { Target width in abstract units (pixels in case of IBLImage) }
    property TargetWidth: Double read GetTargetWidth;

    { Target width in abstract units (pixels in case of IBLImage) }
    property TargetHeight: Double read GetTargetHeight;

    { Returns the target image or nil if there is no target image.

      Note: The rendering context doesn't own the image, but it increases its
      writer count, which means that the image will not be destroyed even when
      user destroys it during the rendering (in such case it will be destroyed
      after the rendering ends when the writer count goes to zero). This means
      that the rendering context must hold the image and not the pointer to
      the IBLImage passed to either the constructor or Start function. So the
      returned pointer is not the same as the pointer passed to Start, but it
      points to the same impl. }
    property TargetImage: IBLImage read GetTargetImage;

    { The type of this context }
    property ContextType: TBLContextType read GetContextType;

    { Whether the context is a built-in null instance. }
    property IsNone: Boolean read GetIsNone;

    { The number of saved states in the context (0 means no saved states). }
    property SavedStateCount: Integer read GetSavedStateCount;

    { Meta-matrix.

      Meta matrix is a core transformation matrix that is normally not changed
      by transformations applied to the context. Instead it acts as a secondary
      matrix used to create the final transformation matrix from meta and user
      matrices.

      Meta matrix can be used to scale the whole context for HI-DPI rendering
      or to change the orientation of the image being rendered, however, the
      number of use-cases is unlimited.

      To change the meta-matrix you must first change user-matrix and then call
      UserToMeta, which would update meta-matrix and clear user-matrix.

      See UserMatrix and UserToMeta. }
    property MetaMatrix: TBLMatrix2D read GetMetaMatrix;

    { User-matrix.

      User matrix contains all transformations that happened to the rendering
      context unless the context was restored or UserToMeta was called. }
    property UserMatrix: TBLMatrix2D read GetUserMatrix write SetUserMatrix;

    { Rendering hints. }
    property Hints: TBLContextHints read GetHints write SetHints;
    property RenderingQuality: TBLRenderingQuality read GetRenderingQuality write SetRenderingQuality;
    property GradientQuality: TBLGradientQuality read GetGradientQuality write SetGradientQuality;
    property PatternQuality: TBLPatternQuality read GetPatternQuality write SetPatternQuality;

    { Approximation options. }
    property ApproximationOptions: TBLApproximationOptions read GetApproximationOptions;

    { Flatten mode. }
    property FlattenMode: TBLFlattenMode read GetFlattenMode write SetFlattenMode;

    { Tolerance used for curve flattening. }
    property FlattenTolerance: Double read GetFlattenTolerance write SetFlattenTolerance;

    { Composition operator. }
    property CompOp: TBLCompOp read GetCompOp write SetCompOp;

    { Global alpha value. }
    property GlobalAlpha: Double read GetGlobalAlpha write SetGlobalAlpha;

    { Fill style }
    property FillStyle: TBLStyleType read GetFillStyle;

    { Stroke style }
    property StrokeStyle: TBLStyleType read GetStrokeStyle;

    { Fill alpha }
    property FillAlpha: Double read GetFillAlpha write SetFillAlpha;

    { Stroke alpha }
    property StrokeAlpha: Double read GetStrokeAlpha write SetStrokeAlpha;

    { Fill color }
    property FillColor: TBLRgba32 read GetFillColor write SetFillColor;
    property FillColor64: TBLRgba64 read GetFillColor64 write SetFillColor64;
    property FillColorF: TBLRgba read GetFillColorF write SetFillColorF;

    { Stroke color }
    property StrokeColor: TBLRgba32 read GetStrokeColor write SetStrokeColor;
    property StrokeColor64: TBLRgba64 read GetStrokeColor64 write SetStrokeColor64;
    property StrokeColorF: TBLRgba read GetStrokeColorF write SetStrokeColorF;

    { Fill Pattern }
    property FillPattern: IBLPattern read GetFillPattern write SetFillPattern;

    { Stroke Pattern }
    property StrokePattern: IBLPattern read GetStrokePattern write SetStrokePattern;

    { Fill Gradient }
    property FillGradient: IBLGradient read GetFillGradient write SetFillGradient;

    { Stroke Gradient }
    property StrokeGradient: IBLGradient read GetStrokeGradient write SetStrokeGradient;

    { Fill rule }
    property FillRule: TBLFillRule read GetFillRule write SetFillRule;

    { Stroke width }
    property StrokeWidth: Double read GetStrokeWidth write SetStrokeWidth;

    { Stroke miter-limit. }
    property StrokeMiterLimit: Double read GetStrokeMiterLimit write SetStrokeMiterLimit;

    { Stroke join }
    property StrokeJoin: TBLStrokeJoin read GetStrokeJoin write SetStrokeJoin;

    { Stroke start-cap }
    property StrokeStartCap: TBLStrokeCap read GetStrokeStartCap write SetStrokeStartCap;

    { Stroke end-cap }
    property StrokeEndCap: TBLStrokeCap read GetStrokeEndCap write SetStrokeEndCap;

    { Stroke dash-offset. }
    property StrokeDashOffset: Double read GetStrokeDashOffset write SetStrokeDashOffset;

    { Stroke dash-array. }
    property StrokeDashArray: TArray<Double> read GetStrokeDashArray write SetStrokeDashArray;

    { Stroke transform order }
    property StrokeTransformOrder: TBLStrokeTransformOrder read GetStrokeTransformOrder write SetStrokeTransformOrder;

    { All stroke options }
    property StrokeOptions: TBLStrokeOptions read GetStrokeOptions write SetStrokeOptions;

    { Internal handle for use with the C API }
    property Handle: PBLContextCore read GetHandle;
  end;

type
  { Implements IBLContext }
  TBLContext = class(TInterfacedObject, IBLContext)
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLContextCore;
  protected
    { IBLContext }
    function GetTargetSize: TBLSize;
    function GetTargetWidth: Double;
    function GetTargetHeight: Double;
    function GetTargetImage: IBLImage;
    function GetContextType: TBLContextType;
    function GetIsNone: Boolean;
    function GetSavedStateCount: Integer;
    function GetMetaMatrix: TBLMatrix2D;
    function GetUserMatrix: TBLMatrix2D;
    procedure SetUserMatrix(const AValue: TBLMatrix2D);
    function GetHints: TBLContextHints;
    procedure SetHints(const AValue: TBLContextHints);
    function GetRenderingQuality: TBLRenderingQuality;
    procedure SetRenderingQuality(const AValue: TBLRenderingQuality);
    function GetGradientQuality: TBLGradientQuality;
    procedure SetGradientQuality(const AValue: TBLGradientQuality);
    function GetPatternQuality: TBLPatternQuality;
    procedure SetPatternQuality(const AValue: TBLPatternQuality);
    function GetApproximationOptions: TBLApproximationOptions;
    function GetFlattenMode: TBLFlattenMode;
    procedure SetFlattenMode(const AValue: TBLFlattenMode);
    function GetFlattenTolerance: Double;
    procedure SetFlattenTolerance(const AValue: Double);
    function GetCompOp: TBLCompOp;
    procedure SetCompOp(const AValue: TBLCompOp);
    function GetGlobalAlpha: Double;
    procedure SetGlobalAlpha(const AValue: Double);
    function GetFillStyle: TBLStyleType; overload;
    function GetStrokeStyle: TBLStyleType; overload;
    function GetFillAlpha: Double;
    procedure SetFillAlpha(const AValue: Double);
    function GetStrokeAlpha: Double;
    procedure SetStrokeAlpha(const AValue: Double);
    function GetFillColor: TBLRgba32;
    procedure SetFillColor(const AValue: TBLRgba32);
    function GetFillColor64: TBLRgba64;
    procedure SetFillColor64(const AValue: TBLRgba64);
    function GetFillColorF: TBLRgba;
    procedure SetFillColorF(const AValue: TBLRgba);
    function GetStrokeColor: TBLRgba32;
    procedure SetStrokeColor(const AValue: TBLRgba32);
    function GetStrokeColor64: TBLRgba64;
    procedure SetStrokeColor64(const AValue: TBLRgba64);
    function GetStrokeColorF: TBLRgba;
    procedure SetStrokeColorF(const AValue: TBLRgba);
    function GetFillPattern: IBLPattern;
    procedure SetFillPattern(const AValue: IBLPattern);
    function GetStrokePattern: IBLPattern;
    procedure SetStrokePattern(const AValue: IBLPattern);
    function GetFillGradient: IBLGradient;
    procedure SetFillGradient(const AValue: IBLGradient);
    function GetStrokeGradient: IBLGradient;
    procedure SetStrokeGradient(const AValue: IBLGradient);
    function GetFillRule: TBLFillRule;
    procedure SetFillRule(const AValue: TBLFillRule);
    function GetStrokeWidth: Double;
    procedure SetStrokeWidth(const AValue: Double);
    function GetStrokeMiterLimit: Double;
    procedure SetStrokeMiterLimit(const AValue: Double);
    function GetStrokeJoin: TBLStrokeJoin;
    procedure SetStrokeJoin(const AValue: TBLStrokeJoin);
    function GetStrokeStartCap: TBLStrokeCap;
    procedure SetStrokeStartCap(const AValue: TBLStrokeCap);
    function GetStrokeEndCap: TBLStrokeCap;
    procedure SetStrokeEndCap(const AValue: TBLStrokeCap);
    function GetStrokeDashOffset: Double;
    procedure SetStrokeDashOffset(const AValue: Double);
    function GetStrokeDashArray: TArray<Double>;
    procedure SetStrokeDashArray(const AValue: TArray<Double>);
    function GetStrokeTransformOrder: TBLStrokeTransformOrder;
    procedure SetStrokeTransformOrder(const AValue: TBLStrokeTransformOrder);
    function GetStrokeOptions: TBLStrokeOptions;
    procedure SetStrokeOptions(const AValue: TBLStrokeOptions);
    function GetHandle: PBLContextCore;

    procedure Reset;
    function Equals(const AOther: IBLContext): Boolean; reintroduce; overload;

    procedure Start(const AImage: IBLImage); overload;
    procedure Start(const AImage: IBLImage;
      const ACreateInfo: TBLContextCreateInfo); overload;
    procedure Finish;
    procedure Flush(const AFlags: TBLContextFlushFlags);

    function QueryThreadCount: Integer;
    function QueryAccumulatedErrorFlags: TBLContextErrorFlags;

    procedure Save; overload;
    procedure Save(out ACookie: TBLContextCookie); overload;
    procedure Restore; overload;
    procedure Restore(const ACookie: TBLContextCookie); overload;

    procedure ResetMatrix;

    procedure Translate(const AX, AY: Double); overload;
    procedure Translate(const APoint: TBLPointI); overload;
    procedure Translate(const APoint: TBLPoint); overload;
    procedure Scale(const AXY: Double); overload;
    procedure Scale(const AX, AY: Double); overload;
    procedure Scale(const APoint: TBLPointI); overload;
    procedure Scale(const APoint: TBLPoint); overload;
    procedure Skew(const AX, AY: Double); overload;
    procedure Skew(const APoint: TBLPoint); overload;
    procedure Rotate(const AAngle: Double); overload;
    procedure Rotate(const AAngle, AX, AY: Double); overload;
    procedure Rotate(const AAngle: Double; const APoint: TBLPointI); overload;
    procedure Rotate(const AAngle: Double; const APoint: TBLPoint); overload;
    procedure Transform(const AMatrix: TBLMatrix2D);

    procedure PostTranslate(const AX, AY: Double); overload;
    procedure PostTranslate(const APoint: TBLPointI); overload;
    procedure PostTranslate(const APoint: TBLPoint); overload;
    procedure PostScale(const AXY: Double); overload;
    procedure PostScale(const AX, AY: Double); overload;
    procedure PostScale(const APoint: TBLPointI); overload;
    procedure PostScale(const APoint: TBLPoint); overload;
    procedure PostSkew(const AX, AY: Double); overload;
    procedure PostSkew(const APoint: TBLPoint); overload;
    procedure PostRotate(const AAngle: Double); overload;
    procedure PostRotate(const AAngle, AX, AY: Double); overload;
    procedure PostRotate(const AAngle: Double; const APoint: TBLPointI); overload;
    procedure PostRotate(const AAngle: Double; const APoint: TBLPoint); overload;
    procedure PostTransform(const AMatrix: TBLMatrix2D);

    procedure UserToMeta;

    procedure GetFillStyle(out ARgba: TBLRgba32); overload;
    procedure GetFillStyle(out ARgba: TBLRgba64); overload;
    procedure GetFillStyle(out ARgba: TBLRgba); overload;
    procedure GetFillStyle(out APattern: IBLPattern); overload;
    procedure GetFillStyle(out AGradient: IBLGradient); overload;

    procedure SetFillStyle(const ARgba: TBLRgba32); overload;
    procedure SetFillStyle(const ARgba: TBLRgba64); overload;
    procedure SetFillStyle(const ARgba: TBLRgba); overload;
    procedure SetFillStyle(const APattern: IBLPattern); overload;
    procedure SetFillStyle(const AGradient: IBLGradient); overload;
    procedure SetFillStyle(const AImage: IBLImage); overload;

    procedure GetStrokeStyle(out ARgba: TBLRgba32); overload;
    procedure GetStrokeStyle(out ARgba: TBLRgba64); overload;
    procedure GetStrokeStyle(out ARgba: TBLRgba); overload;
    procedure GetStrokeStyle(out APattern: IBLPattern); overload;
    procedure GetStrokeStyle(out AGradient: IBLGradient); overload;

    procedure SetStrokeStyle(const ARgba: TBLRgba32); overload;
    procedure SetStrokeStyle(const ARgba: TBLRgba64); overload;
    procedure SetStrokeStyle(const ARgba: TBLRgba); overload;
    procedure SetStrokeStyle(const APattern: IBLPattern); overload;
    procedure SetStrokeStyle(const AGradient: IBLGradient); overload;
    procedure SetStrokeStyle(const AImage: IBLImage); overload;

    procedure RestoreClipping;
    procedure ClipToRect(const ARect: TBLRectI); overload;
    procedure ClipToRect(const ARect: TBLRect); overload;
    procedure ClipToRect(const AX, AY, AW, AH: Double); overload;

    procedure ClearAll;
    procedure ClearRect(const ARect: TBLRectI); overload;
    procedure ClearRect(const ARect: TBLRect); overload;
    procedure ClearRect(const AX, AY, AW, AH: Double); overload;

    procedure FillAll;

    procedure FillBox(const ABox: TBLBoxI); overload;
    procedure FillBox(const ABox: TBLBox); overload;
    procedure FillBox(const AX0, AY0, AX1, AY1: Double); overload;

    procedure FillRect(const ARect: TBLRectI); overload;
    procedure FillRect(const ARect: TBLRect); overload;
    procedure FillRect(const AX, AY, AW, AH: Double); overload;

    procedure FillCircle(const ACircle: TBLCircle); overload;
    procedure FillCircle(const ACX, ACY, AR: Double); overload;

    procedure FillEllipse(const AEllipse: TBLEllipse); overload;
    procedure FillEllipse(const ACX, ACY, ARX, ARY: Double); overload;

    procedure FillRoundRect(const ARoundRect: TBLRoundRect); overload;
    procedure FillRoundRect(const ARect: TBLRect; const AR: Double); overload;
    procedure FillRoundRect(const ARect: TBLRect; const ARX, ARY: Double); overload;
    procedure FillRoundRect(const AX, AY, AW, AH, AR: Double); overload;
    procedure FillRoundRect(const AX, AY, AW, AH, ARX, ARY: Double); overload;

    procedure FillChord(const AChord: TBLArc); overload;
    procedure FillChord(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure FillChord(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    procedure FillPie(const APie: TBLArc); overload;
    procedure FillPie(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure FillPie(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    procedure FillTriangle(const ATriangle: TBLTriangle); overload;
    procedure FillTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload;

    procedure FillPolygon(const APoly: TArray<TBLPoint>); overload;
    procedure FillPolygon(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure FillPolygon(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure FillPolygon(const APoly: TArray<TBLPointI>); overload;
    procedure FillPolygon(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure FillPolygon(const APoly: PBLPointI; const ACount: Integer); overload;

    procedure FillBoxArray(const ABoxes: TArray<TBLBox>); overload;
    procedure FillBoxArray(const ABoxes: TBLArrayView<TBLBox>); overload;
    procedure FillBoxArray(const ABoxes: PBLBox; const ACount: Integer); overload;
    procedure FillBoxArray(const ABoxes: TArray<TBLBoxI>); overload;
    procedure FillBoxArray(const ABoxes: TBLArrayView<TBLBoxI>); overload;
    procedure FillBoxArray(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    procedure FillRectArray(const ARects: TArray<TBLRect>); overload;
    procedure FillRectArray(const ARects: TBLArrayView<TBLRect>); overload;
    procedure FillRectArray(const ARects: PBLRect; const ACount: Integer); overload;
    procedure FillRectArray(const ARects: TArray<TBLRectI>); overload;
    procedure FillRectArray(const ARects: TBLArrayView<TBLRectI>); overload;
    procedure FillRectArray(const ARects: PBLRectI; const ACount: Integer); overload;

    procedure FillRegion(const ARegion: IBLRegion);

    procedure FillPath(const APath: IBLPath);

    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: String); overload;
    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure FillText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UCS4String); overload;
    procedure FillText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UCS4String); overload;

    procedure FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: PBLGlyphRun); overload;
    procedure FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: PBLGlyphRun); overload;

    procedure StrokeBox(const ABox: TBLBoxI); overload;
    procedure StrokeBox(const ABox: TBLBox); overload;
    procedure StrokeBox(const AX0, AY0, AX1, AY1: Double); overload;

    procedure StrokeRect(const ARect: TBLRectI); overload;
    procedure StrokeRect(const ARect: TBLRect); overload;
    procedure StrokeRect(const AX, AY, AW, AH: Double); overload;

    procedure StrokeLine(const ALine: TBLLine); overload;
    procedure StrokeLine(const AP0, AP1: TBLPoint); overload;
    procedure StrokeLine(const AX0, AY0, AX1, AY1: Double); overload;

    procedure StrokeCircle(const ACircle: TBLCircle); overload;
    procedure StrokeCircle(const ACX, ACY, AR: Double); overload;

    procedure StrokeEllipse(const AEllipse: TBLEllipse); overload;
    procedure StrokeEllipse(const ACX, ACY, ARX, ARY: Double); overload;

    procedure StrokeRoundRect(const ARoundRect: TBLRoundRect); overload;
    procedure StrokeRoundRect(const ARect: TBLRect; const AR: Double); overload;
    procedure StrokeRoundRect(const ARect: TBLRect; const ARX, ARY: Double); overload;
    procedure StrokeRoundRect(const AX, AY, AW, AH, AR: Double); overload;
    procedure StrokeRoundRect(const AX, AY, AW, AH, ARX, ARY: Double); overload;

    procedure StrokeArc(const AArc: TBLArc); overload;
    procedure StrokeArc(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokeArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    procedure StrokeChord(const AChord: TBLArc); overload;
    procedure StrokeChord(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokeChord(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    procedure StrokePie(const APie: TBLArc); overload;
    procedure StrokePie(const ACX, ACY, AR, AStart, ASweep: Double); overload;
    procedure StrokePie(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload;

    procedure StrokeTriangle(const ATriangle: TBLTriangle); overload;
    procedure StrokeTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload;

    procedure StrokePolyline(const APoly: TArray<TBLPoint>); overload;
    procedure StrokePolyline(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure StrokePolyline(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure StrokePolyline(const APoly: TArray<TBLPointI>); overload;
    procedure StrokePolyline(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure StrokePolyline(const APoly: PBLPointI; const ACount: Integer); overload;

    procedure StrokePolygon(const APoly: TArray<TBLPoint>); overload;
    procedure StrokePolygon(const APoly: TBLArrayView<TBLPoint>); overload;
    procedure StrokePolygon(const APoly: PBLPoint; const ACount: Integer); overload;
    procedure StrokePolygon(const APoly: TArray<TBLPointI>); overload;
    procedure StrokePolygon(const APoly: TBLArrayView<TBLPointI>); overload;
    procedure StrokePolygon(const APoly: PBLPointI; const ACount: Integer); overload;

    procedure StrokeBoxArray(const ABoxes: TArray<TBLBox>); overload;
    procedure StrokeBoxArray(const ABoxes: TBLArrayView<TBLBox>); overload;
    procedure StrokeBoxArray(const ABoxes: PBLBox; const ACount: Integer); overload;
    procedure StrokeBoxArray(const ABoxes: TArray<TBLBoxI>); overload;
    procedure StrokeBoxArray(const ABoxes: TBLArrayView<TBLBoxI>); overload;
    procedure StrokeBoxArray(const ABoxes: PBLBoxI; const ACount: Integer); overload;

    procedure StrokeRectArray(const ARects: TArray<TBLRect>); overload;
    procedure StrokeRectArray(const ARects: TBLArrayView<TBLRect>); overload;
    procedure StrokeRectArray(const ARects: PBLRect; const ACount: Integer); overload;
    procedure StrokeRectArray(const ARects: TArray<TBLRectI>); overload;
    procedure StrokeRectArray(const ARects: TBLArrayView<TBLRectI>); overload;
    procedure StrokeRectArray(const ARects: PBLRectI; const ACount: Integer); overload;

    procedure StrokePath(const APath: IBLPath);

    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: String); overload;
    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UTF8String); overload;
    procedure StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
      const AText: UCS4String); overload;
    procedure StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
      const AText: UCS4String); overload;

    procedure StrokeGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;
    procedure StrokeGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
      const AGlyphRun: TBLGlyphRun); overload;

    procedure BlitImage(const ADst: TBLPoint; const ASrc: IBLImage); overload;
    procedure BlitImage(const ADst: TBLPointI; const ASrc: IBLImage); overload;

    procedure BlitImage(const ADst: TBLPoint; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;
    procedure BlitImage(const ADst: TBLPointI; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;

    procedure BlitImage(const ADst: TBLRect; const ASrc: IBLImage); overload;
    procedure BlitImage(const ADst: TBLRectI; const ASrc: IBLImage); overload;

    procedure BlitImage(const ADst: TBLRect; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;
    procedure BlitImage(const ADst: TBLRectI; const ASrc: IBLImage;
      const ASrcArea: TBLRectI); overload;
  public
    procedure AfterConstruction; override;
  {$ENDREGION 'Internal Declarations'}
  public
    { Creates a default constructed rendering context.

      Default constructed means that the instance is valid, but uninitialized,
      which means the rendering context does not have attached any target. Any
      attempt to use uninitialized context results in
      TBLResultCode.NotInitialized error. }
    constructor Create; overload;

    { Creates a new rendering context for rendering to the image ATarget. }
    constructor Create(const ATarget: IBLImage); overload;

    { Creates a new rendering context for rendering to the image ATarget.

      This overload accepts create options that can be used to change the
      implementation of the rendering context. }
    constructor Create(const ATarget: IBLImage;
      const ACreateInfo: TBLContextCreateInfo); overload;

    destructor Destroy; override;

    function Equals(Obj: TObject): Boolean; overload; override;
  end;
{$ENDREGION 'Context'}

{$REGION 'Runtime'}

{ ============================================================================
   [Constants]
  ============================================================================ }

{ Blend2D runtime limits.

  Note: These constanst are used across Blend2D, but they are not designed to
  be ABI stable. New versions of Blend2D can increase certain limits without
  notice. Use runtime to query the limits dynamically, see
  TBLRuntimeBuildInfo. }

const
  { Maximum width and height of an image. }
  BL_MAX_IMAGE_SIZE   = BL_RUNTIME_MAX_IMAGE_SIZE;

  { Maximum number of threads for asynchronous operations (including rendering). }
  BL_MAX_THREAD_COUNT =  BL_RUNTIME_MAX_THREAD_COUNT;

{ ============================================================================
   [Enums]
  ============================================================================ }

type
  { Blend2D runtime build type. }
  TBLRuntimeBuildType = (
    { Describes a Blend2D debug build. }
    Debug   = BL_RUNTIME_BUILD_TYPE_DEBUG,

    { Describes a Blend2D release build. }
    Release = BL_RUNTIME_BUILD_TYPE_RELEASE);

type
  { CPU architectures }
  TBLRuntimeCpuArch = (
    { Unknown architecture. }
    Unknown = BL_RUNTIME_CPU_ARCH_UNKNOWN,

    { 32-bit or 64-bit X86 architecture. }
    X86     = BL_RUNTIME_CPU_ARCH_X86,

    { 32-bit or 64-bit ARM architecture. }
    ARM     = BL_RUNTIME_CPU_ARCH_ARM,

    { 32-bit or 64-bit MIPS architecture. }
    MIPS    = BL_RUNTIME_CPU_ARCH_MIPS);

type
  {! CPU features Blend2D supports. }
  TBLRuntimeCpuFeature = (
    SSE2   = 0,
    SSE3   = 1,
    SSSE3  = 2,
    SSE4_1 = 3,
    SSE4_2 = 4,
    AVX    = 5,
    AVX2   = 6);
  TBLRuntimeCpuFeatures = set of TBLRuntimeCpuFeature;

type
  { Runtime cleanup flags that can be used through BLRuntime.Cleanup }
  TBLRuntimeCleanupFlag = (
    { Cleanup object memory pool. }
    ObjectPool = 0,

    { Cleanup zeroed memory pool. }
    ZeroedPool = 1,

    { Cleanup thread pool (would join unused threads). }
    ThreadPool = 4);
  TBLRuntimeCleanupFlags = set of TBLRuntimeCleanupFlag;

type
  _TBLRuntimeCleanupFlagsHelper = record helper for TBLRuntimeCleanupFlags
  public const
    Everything = [TBLRuntimeCleanupFlag.ObjectPool,
                  TBLRuntimeCleanupFlag.ZeroedPool,
                  TBLRuntimeCleanupFlag.ThreadPool];
  end;

{ ============================================================================
   [BLRuntime - BuildInfo]
  ============================================================================ }

type
  { Blend2D build information. }
  TBLRuntimeBuildInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRuntimeBuildInfo;
    function GetBuildType: TBLRuntimeBuildType; inline;
    function GetBaselineCpuFeatures: TBLRuntimeCpuFeatures; inline;
    function GetSupportedCpuFeatures: TBLRuntimeCpuFeatures; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Blend2D version stored as (Major shl 16) or (Minor shl 8) or Patch }
    property Version: Cardinal read FHandle.version.version;

    { Decomposed Blend2D version }
    property PatchVersion: Byte read FHandle.version.patchVersion;
    property MinorVersion: Byte read FHandle.version.minorVersion;
    property MajorVersion: Word read FHandle.version.majorVersion;

    { Blend2D build type }
    property BuildType: TBLRuntimeBuildType read GetBuildType;

    { Baseline CPU features.

      These features describe CPU features that were detected at compile-time.
      Baseline features are used to compile all source files so they represent
      the minimum feature-set the target CPU must support to run Blend2D.

      Official Blend2D builds set baseline at SSE2 on X86 target and NEON on
      ARM target. Custom builds can set use different baseline. }
    property BaselineCpuFeatures: TBLRuntimeCpuFeatures read GetBaselineCpuFeatures;

    { Supported CPU features.

      These features do not represent the features that the host CPU must
      support, instead, they represent all features that Blend2D can take
      advantage of in C++ code that uses instruction intrinsics. For example if
      AVX2 is part of SupportedCpuFeatures it means that Blend2D can take
      advantage of it if there is a separate code-path. }
    property SupportedCpuFeatures: TBLRuntimeCpuFeatures read GetSupportedCpuFeatures;

    { Maximum size of an image (both width and height). }
    property MaxImageSize: Integer read FHandle.maxImageSize;

    { Maximum number of threads for asynchronous operations, including
      rendering. }
    property MaxThreadCount: Integer read FHandle.maxThreadCount;
  end;
  PBLRuntimeBuildInfo = ^TBLRuntimeBuildInfo;

{ ============================================================================
   [BLRuntime - SystemInfo]
  ============================================================================ }

type
  { System information queried by the runtime. }
  TBLRuntimeSystemInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRuntimeSystemInfo;
    function GetCpuArch: TBLRuntimeCpuArch; inline;
    function GetCpuFeatures: TBLRuntimeCpuFeatures; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    { Host CPU architecture }
    property CpuArch: TBLRuntimeCpuArch read GetCpuArch;

    { Host CPU features }
    property CpuFeatures: TBLRuntimeCpuFeatures read GetCpuFeatures;

    { Number of cores of the host CPU/CPUs. }
    property CoreCount: Integer read FHandle.coreCount;

    { Number of threads of the host CPU/CPUs. }
    property ThreadCount: Integer read FHandle.threadCount;

    { Minimum stack size of a worker thread used by Blend2D. }
    property ThreadStackSize: Integer read FHandle.threadStackSize;

    { Allocation granularity of virtual memory (includes thread's stack). }
    property AllocationGranularity: Integer read FHandle.allocationGranularity;
  end;
  PBLRuntimeSystemInfo = ^TBLRuntimeSystemInfo;

{ ============================================================================
   [BLRuntime - MemoryInfo]
  ============================================================================ }

type
  { Provides information about resources allocated by Blend2D. }
  TBLRuntimeResourceInfo = record
  {$REGION 'Internal Declarations'}
  private
    FHandle: BLRuntimeResourceInfo;
  {$ENDREGION 'Internal Declarations'}
  public
    { Virtual memory used at this time. }
    property VMUsed: NativeInt read FHandle.vmUsed;

    { Virtual memory reserved (allocated internally). }
    property VMReserved: NativeInt read FHandle.vmReserved;

    { Overhead required to manage virtual memory allocations. }
    property VMOverhead: NativeInt read FHandle.vmOverhead;

    { Number of blocks of virtual memory allocated. }
    property VMBlockCount: NativeInt read FHandle.vmBlockCount;

    { Zeroed memory used at this time. }
    property ZMUsed: NativeInt read FHandle.zmUsed;

    { Zeroed memory reserved (allocated internally). }
    property ZMReserved: NativeInt read FHandle.zmReserved;

    { Overhead required to manage zeroed memory allocations. }
    property ZMOverhead: NativeInt read FHandle.zmOverhead;

    { Number of blocks of zeroed memory allocated. }
    property ZMBlockCount: NativeInt read FHandle.zmBlockCount;

    { Count of dynamic pipelines created and cached. }
    property DynamicPipelineCount: NativeInt read FHandle.dynamicPipelineCount;

    { Number of active file handles used by Blend2D.

      Note: File handles are counted by IBLFile - when a file is opened a global
      counter is incremented and when it's closed it's decremented.
      This means that this number represents the actual use of IBLFile and
      doesn't consider the origin of the use (it's either Blend2D or user). }
    property FileHandleCount: NativeInt read FHandle.fileHandleCount;

    { Number of active file mappings used by Blend2D.

      Note: Blend2D maps file content to TBytes container, so this number
      represents the actual number of TBytes instances that contain a mapped
      file. }
    property FileMappingCount: NativeInt read FHandle.fileMappingCount;
  end;
  PBLRuntimeMemoryInfo = ^TBLRuntimeResourceInfo;

{ ============================================================================
   [BLRuntime]
  ============================================================================ }

type
  TBLRuntime = class // static
  public
    class procedure Cleanup(const AFlags: TBLRuntimeCleanupFlags); static;
    class procedure QueryBuildInfo(out AInfo: TBLRuntimeBuildInfo); static;
    class procedure QuerySystemInfo(out AInfo: TBLRuntimeSystemInfo); static;
    class procedure QueryResourceInfo(out AInfo: TBLRuntimeResourceInfo); static;
    class procedure LogMessage(const AMessage: String); overload; static;
    class procedure LogMessage(const AMessage: String; const AArgs: array of const); overload; static;
  end;

{$ENDREGION 'Runtime'}

{$REGION 'Internal'}

procedure _BLCheck(const AResult: BLResultCode);

{$ENDREGION 'Internal'}

implementation

uses
  System.TypInfo;

{$POINTERMATH ON}

{$REGION 'Utils'}

type
  IBLArray = interface
  ['{F0CABAA5-F756-483D-95F1-4EBC134793D9}']
    function Handle: PBLArrayCore;
    procedure RevokeOwnership;
  end;

type
  TBLArray = class(TInterfacedObject, IBLArray)
  private
    FHandle: BLArrayCore;
    FIsReference: Boolean;
  protected
    { IBLArray }
    function Handle: PBLArrayCore;
    procedure RevokeOwnership;
  public
    constructor Create(const AType: BLImplType);
    destructor Destroy; override;
  end;

type
  TBLUtils = class // static
  public
    class function ArrayElementType<T>: BLImplType; static;
    class function CreateBLArray<T: record>: IBLArray; static;
    class function BLArrayToArray<T: record>(const AArray: BLArrayCore): TArray<T>; overload; static;
    class function BLArrayToArray<T: record>(const AArray: IBLArray): TArray<T>; overload; static;
    class function ArrayToBLArray<T: record>(const AArray: TArray<T>): IBLArray; static;
  end;

{ TBLArray }

constructor TBLArray.Create(const AType: BLImplType);
begin
  inherited Create;
  blArrayInit(@FHandle, AType);
end;

destructor TBLArray.Destroy;
begin
  if (not FIsReference) then
    blArrayDestroy(@FHandle);
  inherited;
end;

function TBLArray.Handle: PBLArrayCore;
begin
  Result := @FHandle;
end;

procedure TBLArray.RevokeOwnership;
begin
  FIsReference := True;
end;

{ TBLUtils }

class function TBLUtils.ArrayElementType<T>: BLImplType;
var
  Info: PTypeInfo;
  Data: PTypeData;
begin
  Info := TypeInfo(T);
  case GetTypeKind(T) of
    tkInteger,
    tkEnumeration:
      case GetTypeData(Info)^.OrdType of
        otSByte: Exit(BL_IMPL_TYPE_ARRAY_I8);
        otUByte: Exit(BL_IMPL_TYPE_ARRAY_U8);
        otSWord: Exit(BL_IMPL_TYPE_ARRAY_I16);
        otUWord: Exit(BL_IMPL_TYPE_ARRAY_U16);
        otSLong: Exit(BL_IMPL_TYPE_ARRAY_I32);
        otULong: Exit(BL_IMPL_TYPE_ARRAY_U32);
      end;

    tkInt64:
      begin
        Data := GetTypeData(Info);
        if (Data^.MaxInt64Value > Data^.MinInt64Value) then
          Exit(BL_IMPL_TYPE_ARRAY_I64)
        else
          Exit(BL_IMPL_TYPE_ARRAY_U64);
      end;

    tkFloat:
      begin
        Data := GetTypeData(Info);
        case Data^.FloatType of
          ftSingle  : Exit(BL_IMPL_TYPE_ARRAY_F32);
          ftDouble  : Exit(BL_IMPL_TYPE_ARRAY_F64);
        end;
      end;

    tkRecord:
      case SizeOf(T) of
         1: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_1);
         2: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_2);
         3: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_3);
         4: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_4);
         6: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_6);
         8: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_8);
        10: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_10);
        12: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_12);
        16: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_16);
        20: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_20);
        24: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_24);
        32: Exit(BL_IMPL_TYPE_ARRAY_STRUCT_32);
      end;
  end;

  Assert(False, 'Unsupported array element type');
  Result := BL_IMPL_TYPE_NULL;
end;

class function TBLUtils.ArrayToBLArray<T>(const AArray: TArray<T>): IBLArray;
var
  A: PBLArrayCore;
  Count: Integer;
begin
  Result := TBLArray.Create(ArrayElementType<T>);

  Count := Length(AArray);
  if (Count <> 0) then
  begin
    A := Result.Handle;
    _BLCheck(blArrayReserve(A, Count));
    _BLCheck(blArrayAppendView(A, @AArray[0], Count));
  end;
end;

class function TBLUtils.BLArrayToArray<T>(const AArray: BLArrayCore): TArray<T>;
var
  Impl: PBLArrayImpl;
  Count: Integer;
begin
  Impl := AArray.impl;
  Assert(Assigned(Impl) and (Impl.itemSize = SizeOf(T)));

  Count := Impl.size;
  if (Count = 0) then
    Exit(nil);

  SetLength(Result, Count);
  Move(Impl.data^, Result[0], Count * SizeOf(T));
end;

class function TBLUtils.BLArrayToArray<T>(const AArray: IBLArray): TArray<T>;
var
  Impl: PBLArrayImpl;
  Count: Integer;
begin
  Impl := AArray.Handle.impl;
  Assert(Assigned(Impl) and (Impl.itemSize = SizeOf(T)));

  Count := Impl.size;
  if (Count = 0) then
    Exit(nil);

  SetLength(Result, Count);
  Move(Impl.data^, Result[0], Count * SizeOf(T));
end;

class function TBLUtils.CreateBLArray<T>: IBLArray;
begin
  Result := TBLArray.Create(ArrayElementType<T>);
end;

{$ENDREGION 'Utils'}

{$REGION 'Error Handling'}

var
  GErrorHandler: TBLErrorHandler = nil;
  GErrorUserData: Pointer = nil;
  GLastError: TBLResultCode = TBLResultCode.Success;

procedure BLSetErrorHandler(const AHandler: TBLErrorHandler;
  const AUserData: Pointer);
begin
  GErrorHandler := AHandler;
  GErrorUserData := AUserData;
end;

procedure ExceptionErrorHandler(const AResultCode: TBLResultCode;
  const AUserData: Pointer);
begin
  raise EBlend2DError.Create(AResultCode);
end;

procedure BLSetExceptionErrorHandler;
begin
  GErrorHandler := ExceptionErrorHandler;
  GErrorUserData := nil;
end;

procedure GetLastErrorHandler(const AResultCode: TBLResultCode;
  const AUserData: Pointer);
begin
  GLastError := AResultCode;
end;

function BLGetLastError: TBLResultCode;
begin
  Result := GLastError;
  GLastError := TBLResultCode.Success;
end;

procedure BLSetGetLastErrorHandler;
begin
  GErrorHandler := GetLastErrorHandler;
  GErrorUserData := nil;
end;

{ _TBLResultCodeHelper }

function _TBLResultCodeHelper.ToString: String;
const
  ERROR_STRINGS: array [BL_ERROR_START_INDEX..Ord(High(TBLResultCode))] of String = (
    'Out of memory',
    'Invalid value/argument',
    'Invalid state',
    'Invalid handle or file.',
    'Value too large',
    'Not initialized (some instance is built-in none when it shouldn''t be).',
    'Not implemented',
    'Operation not permitted',
    'IO error',
    'Device or resource busy',
    'Operation interrupted',
    'Try again',
    'Timed out',
    'Broken pipe',
    'File is not seekable',
    'Too many levels of symlinks',
    'File is too large',
    'File/directory already exists',
    'Access denied',
    'Media changed',
    'The file/FS is read-only',
    'Device doesn''t exist',
    'Not found, no entry (fs)',
    'No media in drive/device',
    'No more data / end of file',
    'No more files',
    'No space left on device',
    'Directory is not empty',
    'Not a file',
    'Not a directory',
    'Not same device',
    'Not a block device',
    'File/path name is invalid',
    'File/path name is too long',
    'Too many open files',
    'Too many open files by OS',
    'Too many symbolic links on FS',
    'Too many threads',
    'Thread pool is exhausted and couldn''t acquire the requested thread count',
    'File is empty (not specific to any OS error).',
    'File open failed',
    'Not a root device/directory',
    'Unknown system error that failed to translate to Blend2D result code.',
    'Invalid data alignment.',
    'Invalid data signature or header.',
    'Invalid or corrupted data.',
    'Invalid string (invalid data of either UTF8, UTF16, or UTF32).',
    'Truncated data (more data required than memory/stream provides).',
    'Input data too large to be processed.',
    'Decompression failed due to invalid data (RLE, Huffman, etc).',
    'Invalid geometry (invalid path data or shape).',
    'Returned when there is no matching vertex in path data.',
    'No matching cookie (BLContext).',
    'No states to restore (BLContext).',
    'The size of the image is too large.',
    'Image codec for a required format doesn''t exist.',
    'Unknown or invalid file format that cannot be read.',
    'Image codec doesn''t support reading the file format.',
    'Image codec doesn''t support writing the file format.',
    'Multiple IHDR chunks are not allowed (PNG).',
    'Invalid IDAT chunk (PNG).',
    'Invalid IEND chunk (PNG).',
    'Invalid PLTE chunk (PNG).',
    'Invalid tRNS chunk (PNG).',
    'Invalid filter type (PNG).',
    'Unsupported feature (JPEG).',
    'Invalid SOS marker or header (JPEG).',
    'Invalid SOF marker (JPEG).',
    'Multiple SOF markers (JPEG).',
    'Unsupported SOF marker (JPEG).',
    'Font doesn''t have any data as it''s not initialized.',
    'Font or font-face was not matched (TBLFontManager).',
    'Font has no character to glyph mapping data.',
    'Font has missing an important table.',
    'Font feature is not available.',
    'Font has an invalid CFF data.',
    'Font program terminated because the execution reached the limit.',
    'Invalid glyph identifier.');
begin
  if (Self = TBLResultCode.Success) then
    Result := 'Success'
  else if (Ord(Self) >= Low(ERROR_STRINGS)) and (Ord(Self) <= High(ERROR_STRINGS)) then
    Result := ERROR_STRINGS[Ord(Self)]
  else
    Result := Format('Unknown error (%d)', [Ord(Self)]);
end;

{ EBlend2DError }

constructor EBlend2DError.Create(const AResultCode: TBLResultCode);
begin
  inherited Create(AResultCode.ToString);
  FResultCode := AResultCode;
end;

{$ENDREGION 'Error Handling'}

{$REGION 'General'}

{ TBLRange }

function BLRange(const AStart, AFinish: Integer): TBLRange; inline;
begin
  Result.Reset(AStart, AFinish);
end;

class operator TBLRange.Equal(const ALeft, ARight: TBLRange): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRange.Equals(const AOther: TBLRange): Boolean;
begin
  Result := (FHandle.start = AOther.FHandle.start) and
            (FHandle.&end = AOther.FHandle.&end);
end;

function TBLRange.GetFinish: Integer;
begin
  Result := FHandle.&end;
end;

function TBLRange.GetStart: Integer;
begin
  Result := FHandle.start;
end;

class operator TBLRange.NotEqual(const ALeft, ARight: TBLRange): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRange.Reset;
begin
  FHandle.start := 0;
  FHandle.&end := 0;
end;

procedure TBLRange.Reset(const AStart, AFinish: Integer);
begin
  FHandle.start := AStart;
  FHandle.&end := AFinish;
end;

procedure TBLRange.SetFinish(const AValue: Integer);
begin
  FHandle.&end := AValue;
end;

procedure TBLRange.SetStart(const AValue: Integer);
begin
  FHandle.start := AValue;
end;

{$ENDREGION 'General'}

{$REGION 'Array'}

{ TBLArrayView<T> }

function TBLArrayView<T>.GetLast: Pointer;
begin
  Result := FHandle.data;
  Inc(P(Result), FHandle.size);
end;

function TBLArrayView<T>.GetLength: Integer;
begin
  Result := FHandle.size;
end;

procedure TBLArrayView<T>.Reset;
begin
  FHandle.data := nil;
  FHandle.size := 0;
end;

procedure TBLArrayView<T>.Reset(const AData: Pointer; const ALength: Integer);
begin
  FHandle.data := AData;
  FHandle.size := ALength;
end;

{$ENDREGION 'Array'}

{$REGION 'Color'}

{ TBLRgba32 }

function BLRgba32: TBLRgba32; overload; inline;
begin
  Result.FHandle.value := 0;
end;

function BLRgba32(const AValue: Cardinal): TBLRgba32; inline;
begin
  Result.FHandle.value := AValue;
end;

function BLRgba32(const AR, AG, AB: Byte; const AA: Byte = 255): TBLRgba32; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLRgba32(const AValue: TBLRgba64): TBLRgba32; overload; inline;
begin
  Result.Reset(AValue);
end;

function BLRgba32(const AValue: TBLRgba): TBLRgba32; overload; inline;
begin
  Result.Reset(AValue);
end;

class operator TBLRgba32.Implicit(const AValue: Cardinal): TBLRgba32;
begin
  Result.FHandle.value := AValue;
end;

class operator TBLRgba32.Equal(const ALeft, ARight: TBLRgba32): Boolean;
begin
  Result := (ALeft.FHandle.value = ARight.FHandle.value);
end;

function TBLRgba32.GetIsOpaque: Boolean;
begin
  Result := (FHandle.value >= $FF000000);
end;

function TBLRgba32.GetIsTransparent: Boolean;
begin
  Result := (FHandle.value <= $00FFFFFF);
end;

class operator TBLRgba32.Implicit(const AValue: TBLRgba32): Cardinal;
begin
  Result := AValue.FHandle.value;
end;

class operator TBLRgba32.NotEqual(const ALeft, ARight: TBLRgba32): Boolean;
begin
  Result := (ALeft.FHandle.value <> ARight.FHandle.value);
end;

procedure TBLRgba32.Reset;
begin
  FHandle.value := 0;
end;

procedure TBLRgba32.Reset(const AValue: Cardinal);
begin
  FHandle.value := AValue;
end;

procedure TBLRgba32.Reset(const AR, AG, AB, AA: Byte);
begin
  FHandle.value := (AA shl 24) or (AR shl 16) or (AG shl 8) or AB;
end;

{ TBLRgba64 }

function BLRgba64: TBLRgba64; overload; inline;
begin
  Result.FHandle.value := 0;
end;

function BLRgba64(const AValue: UInt64): TBLRgba64; overload; inline;
begin
  Result.FHandle.value := AValue;
end;

function BLRgba64(const AR, AG, AB: Word; const AA: Word = $FFFF): TBLRgba64; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLRgba64(const AValue: TBLRgba32): TBLRgba64; overload; inline;
begin
  Result.Reset(AValue);
end;

function BLRgba64(const AValue: TBLRgba): TBLRgba32; overload; inline;
begin
  Result.Reset(AValue);
end;

class operator TBLRgba64.Equal(const ALeft, ARight: TBLRgba64): Boolean;
begin
  Result := (ALeft.FHandle.value = ARight.FHandle.value);
end;

function TBLRgba64.GetIsOpaque: Boolean;
begin
  Result := (FHandle.value >= $FFFF000000000000);
end;

function TBLRgba64.GetIsTransparent: Boolean;
begin
  Result := (FHandle.value <= $0000FFFFFFFFFFFF);
end;

class operator TBLRgba64.Implicit(const AValue: UInt64): TBLRgba64;
begin
  Result.FHandle.value := AValue;
end;

class operator TBLRgba64.Implicit(const AValue: TBLRgba64): UInt64;
begin
  Result := AValue.FHandle.value;
end;

class operator TBLRgba64.NotEqual(const ALeft, ARight: TBLRgba64): Boolean;
begin
  Result := (ALeft.FHandle.value <> ARight.FHandle.value);
end;

procedure TBLRgba64.Reset(const AValue: TBLRgba32);
begin
  FHandle.r := AValue.FHandle.r or (AValue.FHandle.r shl 8);
  FHandle.g := AValue.FHandle.g or (AValue.FHandle.g shl 8);
  FHandle.b := AValue.FHandle.b or (AValue.FHandle.b shl 8);
  FHandle.a := AValue.FHandle.a or (AValue.FHandle.a shl 8);
end;

procedure TBLRgba64.Reset(const AR, AG, AB, AA: Word);
begin
  FHandle.value := (UInt64(AA) shl 48) or (UInt64(AR) shl 32)
                or (UInt64(AG) shl 16) or UInt64(AB);
end;

procedure TBLRgba64.Reset(const AValue: UInt64);
begin
  FHandle.value := AValue;
end;

procedure TBLRgba64.Reset;
begin
  FHandle.value := 0;
end;

{ TBLRgba }

function BLRgba: TBLRgba; overload; inline;
begin
  Result.Reset;
end;

function BLRgba(const AR, AG, AB: Single; const AA: Single = 1): TBLRgba; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLRgba(const ARgba: TBLRgba32): TBLRgba; overload; inline;
begin
  Result.Reset(ARgba);
end;

function BLRgba(const ARgba: TBLRgba64): TBLRgba; overload; inline;
begin
  Result.Reset(ARgba);
end;

class operator TBLRgba.Equal(const ALeft, ARight: TBLRgba): Boolean;
begin
  Result := (ALeft.FHandle.r = ARight.FHandle.r)
        and (ALeft.FHandle.g = ARight.FHandle.g)
        and (ALeft.FHandle.b = ARight.FHandle.b)
        and (ALeft.FHandle.a = ARight.FHandle.a);
end;

class operator TBLRgba.Equal(const ALeft: TBLRgba;
  const ARight: TBLRgba32): Boolean;
begin
  Result := (ALeft = BLRgba(ARight));
end;

class operator TBLRgba.Equal(const ALeft: TBLRgba;
  const ARight: TBLRgba64): Boolean;
begin
  Result := (ALeft = BLRgba(ARight));
end;

function TBLRgba.GetIsOpaque: Boolean;
begin
  Result := (FHandle.a >= 1);
end;

function TBLRgba.GetIsTransparent: Boolean;
begin
  Result := (FHandle.a = 0);
end;

class operator TBLRgba.NotEqual(const ALeft: TBLRgba;
  const ARight: TBLRgba32): Boolean;
begin
  Result := (ALeft <> BLRgba(ARight));
end;

class operator TBLRgba.NotEqual(const ALeft: TBLRgba;
  const ARight: TBLRgba64): Boolean;
begin
  Result := (ALeft <> BLRgba(ARight));
end;

class operator TBLRgba.NotEqual(const ALeft, ARight: TBLRgba): Boolean;
begin
  Result := not (ALeft = ARight);
end;

procedure TBLRgba.Reset(const ARgba: TBLRgba64);
const
  FACTOR = 1.0 / 65535.0;
begin
  FHandle.r := ARgba.R * FACTOR;
  FHandle.g := ARgba.G * FACTOR;
  FHandle.b := ARgba.B * FACTOR;
  FHandle.a := ARgba.A * FACTOR;
end;

procedure TBLRgba.Reset(const ARgba: TBLRgba32);
const
  FACTOR = 1.0 / 255.0;
begin
  FHandle.r := ARgba.R * FACTOR;
  FHandle.g := ARgba.G * FACTOR;
  FHandle.b := ARgba.B * FACTOR;
  FHandle.a := ARgba.A * FACTOR;
end;

procedure TBLRgba.Reset;
begin
  FHandle.r := 0;
  FHandle.g := 0;
  FHandle.b := 0;
  FHandle.a := 0;
end;

procedure TBLRgba.Reset(const AR, AG, AB, AA: Single);
begin
  FHandle.r := AR;
  FHandle.g := AG;
  FHandle.b := AB;
  FHandle.a := AA;
end;

{ _TBLRgba32Helper }

procedure _TBLRgba32Helper.Reset(const AValue: TBLRgba64);
var
  Hi, Lo: UInt32;
begin
  Hi := AValue.FHandle.value shr 32;
  Lo := AValue.FHandle.value;
  FHandle.value :=  (Hi and $FF000000)         or
                   ((Lo and $FF000000) shr 16) or
                   ((Hi and $0000FF00) shl  8) or
                   ((Lo and $0000FF00) shr  8);
end;

procedure _TBLRgba32Helper.Reset(const AValue: TBLRgba);
begin
  FHandle.a := EnsureRange(Trunc(AValue.A * 255.0), 0, 255);
  FHandle.r := EnsureRange(Trunc(AValue.R * 255.0), 0, 255);
  FHandle.g := EnsureRange(Trunc(AValue.G * 255.0), 0, 255);
  FHandle.b := EnsureRange(Trunc(AValue.B * 255.0), 0, 255);
end;

{ _TBLRgba64Helper }

procedure _TBLRgba64Helper.Reset(const AValue: TBLRgba);
begin
  FHandle.a := EnsureRange(Trunc(AValue.A * 65535.0), 0, 65535);
  FHandle.r := EnsureRange(Trunc(AValue.R * 65535.0), 0, 65535);
  FHandle.g := EnsureRange(Trunc(AValue.G * 65535.0), 0, 65535);
  FHandle.b := EnsureRange(Trunc(AValue.B * 65535.0), 0, 65535);
end;

{$ENDREGION 'Color'}

{$REGION 'Geometry'}

function BLPointI: TBLPointI; overload; inline;
begin
  Result.Reset;
end;

function BLPointI(const AX, AY: Integer): TBLPointI; overload; inline;
begin
  Result.Reset(AX, AY);
end;

{ TBLPointI }

class operator TBLPointI.Add(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x + ARight.FHandle.x, ALeft.FHandle.y + ARight.FHandle.y);
end;

class operator TBLPointI.Add(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft + ARight.FHandle.x, ALeft + ARight.FHandle.y);
end;

class operator TBLPointI.Add(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x + ARight, ALeft.FHandle.y + ARight);
end;

class operator TBLPointI.Equal(const ALeft, ARight: TBLPointI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLPointI.Equals(const AOther: TBLPointI): Boolean;
begin
  Result := (FHandle.x = AOther.FHandle.x)
        and (FHandle.y = AOther.FHandle.y);
end;

class operator TBLPointI.IntDivide(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x div ARight, ALeft.FHandle.y div ARight);
end;

class operator TBLPointI.IntDivide(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft div ARight.FHandle.x, ALeft div ARight.FHandle.y);
end;

class operator TBLPointI.IntDivide(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x div ARight.FHandle.x, ALeft.FHandle.y div ARight.FHandle.y);
end;

class operator TBLPointI.Multiply(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x * ARight.FHandle.x, ALeft.FHandle.y * ARight.FHandle.y);
end;

class operator TBLPointI.Multiply(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft * ARight.FHandle.x, ALeft * ARight.FHandle.y);
end;

class operator TBLPointI.Multiply(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x * ARight, ALeft.FHandle.y * ARight);
end;

class operator TBLPointI.Negative(const AValue: TBLPointI): TBLPointI;
begin
  Result.Reset(-AValue.FHandle.x, -AValue.FHandle.y);
end;

class operator TBLPointI.NotEqual(const ALeft, ARight: TBLPointI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLPointI.Reset(const AX, AY: Integer);
begin
  FHandle.x := AX;
  FHandle.y := AY;
end;

class operator TBLPointI.Subtract(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x - ARight, ALeft.FHandle.y - ARight);
end;

class operator TBLPointI.Subtract(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft - ARight.FHandle.x, ALeft - ARight.FHandle.y);
end;

class operator TBLPointI.Subtract(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.FHandle.x - ARight.FHandle.x, ALeft.FHandle.y - ARight.FHandle.y);
end;

procedure TBLPointI.Reset;
begin
  FHandle.x := 0;
  FHandle.y := 0;
end;

{ TBLSizeI }

function BLSizeI: TBLSizeI; overload; inline;
begin
  Result.Reset;
end;

function BLSizeI(const AW, AH: Integer): TBLSizeI; overload; inline;
begin
  Result.Reset(AW, AH);
end;

class operator TBLSizeI.Equal(const ALeft, ARight: TBLSizeI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLSizeI.Equals(const AOther: TBLSizeI): Boolean;
begin
  Result := (FHandle.w = AOther.FHandle.w)
        and (FHandle.h = AOther.FHandle.h);
end;

class operator TBLSizeI.NotEqual(const ALeft, ARight: TBLSizeI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLSizeI.Reset(const AW, AH: Integer);
begin
  FHandle.w := AW;
  FHandle.h := AH;
end;

procedure TBLSizeI.Reset;
begin
  FHandle.w := 0;
  FHandle.h := 0;
end;

{ TBLBoxI }

function BLBoxI: TBLBoxI; overload; inline;
begin
  Result.Reset;
end;

function BLBoxI(const AX0, AY0, AX1, AY1: Integer): TBLBoxI; overload; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function TBLBoxI.Contains(const AX, AY: Integer): Boolean;
begin
  Result := (AX >= FHandle.x0) and (AY >= FHandle.y0)
        and (AX <  FHandle.x1) and (AY <  FHandle.y1);
end;

function TBLBoxI.Contains(const AP: TBLPointI): Boolean;
begin
  Result := (AP.FHandle.x >= FHandle.x0) and (AP.FHandle.y >= FHandle.y0)
        and (AP.FHandle.x <  FHandle.x1) and (AP.FHandle.y <  FHandle.y1);
end;

class operator TBLBoxI.Equal(const ALeft, ARight: TBLBoxI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLBoxI.Equals(const AOther: TBLBoxI): Boolean;
begin
  Result := (FHandle.x0 = AOther.FHandle.x0)
        and (FHandle.y0 = AOther.FHandle.y0)
        and (FHandle.x1 = AOther.FHandle.x1)
        and (FHandle.y1 = AOther.FHandle.y0);
end;

function TBLBoxI.GetHeight: Integer;
begin
  Result := FHandle.y1 - FHandle.y0;
end;

function TBLBoxI.GetWidth: Integer;
begin
  Result := FHandle.x1 - FHandle.y0;
end;

class operator TBLBoxI.NotEqual(const ALeft, ARight: TBLBoxI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLBoxI.Reset(const AX0, AY0, AX1, AY1: Integer);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
end;

procedure TBLBoxI.Reset;
begin
  FHandle.x0 := 0;
  FHandle.y0 := 0;
  FHandle.x1 := 0;
  FHandle.y1 := 0;
end;

procedure TBLBoxI.SetHeight(const AValue: Integer);
begin
  FHandle.y1 := FHandle.y0 + AValue;
end;

procedure TBLBoxI.SetWidth(const AValue: Integer);
begin
  FHandle.x1 := FHandle.x0 + AValue;
end;

{ TBLRectI }

function BLRectI: TBLRectI; inline;
begin
  Result.Reset;
end;

function BLRectI(const AX, AY, AW, AH: Integer): TBLRectI; inline;
begin
  Result.Reset(AX, AY, AW, AH);
end;

class operator TBLRectI.Equal(const ALeft, ARight: TBLRectI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRectI.Equals(const AOther: TBLRectI): Boolean;
begin
  Result := (FHandle.x = AOther.FHandle.x)
        and (FHandle.y = AOther.FHandle.y)
        and (FHandle.w = AOther.FHandle.w)
        and (FHandle.h = AOther.FHandle.h);
end;

class operator TBLRectI.NotEqual(const ALeft, ARight: TBLRectI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRectI.Reset(const AX, AY, AW, AH: Integer);
begin
  FHandle.x := AX;
  FHandle.y := AY;
  FHandle.w := AW;
  FHandle.h := AH;
end;

procedure TBLRectI.Reset;
begin
  FHandle.x := 0;
  FHandle.y := 0;
  FHandle.w := 0;
  FHandle.h := 0;
end;

{ TBLPoint }

function BLPoint: TBLPoint; overload; inline;
begin
  Result.Reset;
end;

function BLPoint(const AX, AY: Double): TBLPoint; inline;
begin
  Result.Reset(AX, AY);
end;

class operator TBLPoint.Add(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x + ARight.FHandle.x, ALeft.FHandle.y + ARight.FHandle.y);
end;

class operator TBLPoint.Divide(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x / ARight, ALeft.FHandle.y / ARight);
end;

class operator TBLPoint.Divide(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft / ARight.FHandle.x, ALeft / ARight.FHandle.y);
end;

class operator TBLPoint.Divide(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x / ARight.FHandle.x, ALeft.FHandle.y / ARight.FHandle.y);
end;

class operator TBLPoint.Add(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft + ARight.FHandle.x, ALeft + ARight.FHandle.y);
end;

class operator TBLPoint.Add(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x + ARight, ALeft.FHandle.y + ARight);
end;

class operator TBLPoint.Equal(const ALeft, ARight: TBLPoint): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLPoint.Equals(const AOther: TBLPoint): Boolean;
begin
  Result := (FHandle.x = AOther.FHandle.x)
        and (FHandle.y = AOther.FHandle.y);
end;

class operator TBLPoint.Multiply(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x * ARight.FHandle.x, ALeft.FHandle.y * ARight.FHandle.y);
end;

class operator TBLPoint.Multiply(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft * ARight.FHandle.x, ALeft * ARight.FHandle.y);
end;

class operator TBLPoint.Multiply(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x * ARight, ALeft.FHandle.y * ARight);
end;

class operator TBLPoint.Negative(const AValue: TBLPoint): TBLPoint;
begin
  Result.Reset(-AValue.FHandle.x, -AValue.FHandle.y);
end;

class operator TBLPoint.NotEqual(const ALeft, ARight: TBLPoint): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLPoint.Reset(const AX, AY: Double);
begin
  FHandle.x := AX;
  FHandle.y := AY;
end;

class operator TBLPoint.Subtract(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x - ARight, ALeft.FHandle.y - ARight);
end;

class operator TBLPoint.Subtract(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft - ARight.FHandle.x, ALeft - ARight.FHandle.y);
end;

class operator TBLPoint.Subtract(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.FHandle.x - ARight.FHandle.x, ALeft.FHandle.y - ARight.FHandle.y);
end;

procedure TBLPoint.Reset;
begin
  FHandle.x := 0;
  FHandle.y := 0;
end;

{ TBLSize }

function BLSize: TBLSize; overload; inline;
begin
  Result.Reset;
end;

function BLSize(const AW, AH: Double): TBLSize; overload; inline;
begin
  Result.Reset(AW, AH);
end;

class operator TBLSize.Equal(const ALeft, ARight: TBLSize): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLSize.Equals(const AOther: TBLSize): Boolean;
begin
  Result := (FHandle.w = AOther.FHandle.w)
        and (FHandle.h = AOther.FHandle.h);
end;

class operator TBLSize.NotEqual(const ALeft, ARight: TBLSize): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLSize.Reset(const AW, AH: Double);
begin
  FHandle.w := AW;
  FHandle.h := AH;
end;

procedure TBLSize.Reset;
begin
  FHandle.w := 0;
  FHandle.h := 0;
end;

{ TBLBox }

function BLBox: TBLBox; overload; inline;
begin
  Result.Reset;
end;

function BLBox(const AX0, AY0, AX1, AY1: Double): TBLBox; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function TBLBox.Contains(const AX, AY: Double): Boolean;
begin
  Result := (AX >= FHandle.x0) and (AY >= FHandle.y0)
        and (AX <  FHandle.x1) and (AY <  FHandle.y1);
end;

class operator TBLBox.Add(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x + ARight.FHandle.x0,
               ALeft.FHandle.y + ARight.FHandle.y0,
               ALeft.FHandle.x + ARight.FHandle.x1,
               ALeft.FHandle.y + ARight.FHandle.y1);
end;

class operator TBLBox.Add(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 + ARight.FHandle.x,
               ALeft.FHandle.y0 + ARight.FHandle.y,
               ALeft.FHandle.x1 + ARight.FHandle.x,
               ALeft.FHandle.y1 + ARight.FHandle.y);
end;

class operator TBLBox.Add(const ALeft: Double; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft + ARight.FHandle.x0,
               ALeft + ARight.FHandle.y0,
               ALeft + ARight.FHandle.x1,
               ALeft + ARight.FHandle.y1);
end;

class operator TBLBox.Add(const ALeft: TBLBox; const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 + ARight,
               ALeft.FHandle.y0 + ARight,
               ALeft.FHandle.x1 + ARight,
               ALeft.FHandle.y1 + ARight);
end;

function TBLBox.Contains(const AP: TBLPoint): Boolean;
begin
  Result := (AP.FHandle.x >= FHandle.x0) and (AP.FHandle.y >= FHandle.y0)
        and (AP.FHandle.x <  FHandle.x1) and (AP.FHandle.y <  FHandle.y1);
end;

class operator TBLBox.Divide(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x / ARight.FHandle.x0,
               ALeft.FHandle.y / ARight.FHandle.y0,
               ALeft.FHandle.x / ARight.FHandle.x1,
               ALeft.FHandle.y / ARight.FHandle.y1);
end;

class operator TBLBox.Divide(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 / ARight.FHandle.x,
               ALeft.FHandle.y0 / ARight.FHandle.y,
               ALeft.FHandle.x1 / ARight.FHandle.x,
               ALeft.FHandle.y1 / ARight.FHandle.y);
end;

class operator TBLBox.Divide(const ALeft: Double; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft / ARight.FHandle.x0,
               ALeft / ARight.FHandle.y0,
               ALeft / ARight.FHandle.x1,
               ALeft / ARight.FHandle.y1);
end;

class operator TBLBox.Divide(const ALeft: TBLBox; const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 / ARight,
               ALeft.FHandle.y0 / ARight,
               ALeft.FHandle.x1 / ARight,
               ALeft.FHandle.y1 / ARight);
end;

class operator TBLBox.Equal(const ALeft, ARight: TBLBox): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLBox.Equals(const AOther: TBLBox): Boolean;
begin
  Result := (FHandle.x0 = AOther.FHandle.x0)
        and (FHandle.y0 = AOther.FHandle.y0)
        and (FHandle.x1 = AOther.FHandle.x1)
        and (FHandle.y1 = AOther.FHandle.y0);
end;

function TBLBox.GetHeight: Double;
begin
  Result := FHandle.y1 - FHandle.y0;
end;

function TBLBox.GetWidth: Double;
begin
  Result := FHandle.x1 - FHandle.x0;
end;

class operator TBLBox.Multiply(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x * ARight.FHandle.x0,
               ALeft.FHandle.y * ARight.FHandle.y0,
               ALeft.FHandle.x * ARight.FHandle.x1,
               ALeft.FHandle.y * ARight.FHandle.y1);
end;

class operator TBLBox.Multiply(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 * ARight.FHandle.x,
               ALeft.FHandle.y0 * ARight.FHandle.y,
               ALeft.FHandle.x1 * ARight.FHandle.x,
               ALeft.FHandle.y1 * ARight.FHandle.y);
end;

class operator TBLBox.Multiply(const ALeft: Double;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft * ARight.FHandle.x0,
               ALeft * ARight.FHandle.y0,
               ALeft * ARight.FHandle.x1,
               ALeft * ARight.FHandle.y1);
end;

class operator TBLBox.Multiply(const ALeft: TBLBox;
  const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 * ARight,
               ALeft.FHandle.y0 * ARight,
               ALeft.FHandle.x1 * ARight,
               ALeft.FHandle.y1 * ARight);
end;

class operator TBLBox.NotEqual(const ALeft, ARight: TBLBox): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLBox.Reset;
begin
  FHandle.x0 := 0;
  FHandle.y0 := 0;
  FHandle.x1 := 0;
  FHandle.y1 := 0;
end;

procedure TBLBox.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
end;

procedure TBLBox.SetHeight(const AValue: Double);
begin
  FHandle.y1 := FHandle.y0 + AValue;
end;

procedure TBLBox.SetWidth(const AValue: Double);
begin
  FHandle.x1 := FHandle.x0 + AValue;
end;

class operator TBLBox.Subtract(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x - ARight.FHandle.x0,
               ALeft.FHandle.y - ARight.FHandle.y0,
               ALeft.FHandle.x - ARight.FHandle.x1,
               ALeft.FHandle.y - ARight.FHandle.y1);
end;

class operator TBLBox.Subtract(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 - ARight.FHandle.x,
               ALeft.FHandle.y0 - ARight.FHandle.y,
               ALeft.FHandle.x1 - ARight.FHandle.x,
               ALeft.FHandle.y1 - ARight.FHandle.y);
end;

class operator TBLBox.Subtract(const ALeft: Double;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft - ARight.FHandle.x0,
               ALeft - ARight.FHandle.y0,
               ALeft - ARight.FHandle.x1,
               ALeft - ARight.FHandle.y1);
end;

class operator TBLBox.Subtract(const ALeft: TBLBox;
  const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.FHandle.x0 - ARight,
               ALeft.FHandle.y0 - ARight,
               ALeft.FHandle.x1 - ARight,
               ALeft.FHandle.y1 - ARight);
end;

{ TBLRect }

function BLRect: TBLRect; overload; inline;
begin
  Result.Reset;
end;

function BLRect(const AX, AY, AW, AH: Double): TBLRect; inline;
begin
  Result.Reset(AX, AY, AW, AH);
end;

class operator TBLRect.Equal(const ALeft, ARight: TBLRect): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRect.Equals(const AOther: TBLRect): Boolean;
begin
  Result := (FHandle.x = AOther.FHandle.x)
        and (FHandle.y = AOther.FHandle.y)
        and (FHandle.w = AOther.FHandle.w)
        and (FHandle.h = AOther.FHandle.h);
end;

class operator TBLRect.NotEqual(const ALeft, ARight: TBLRect): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRect.Reset(const AX, AY, AW, AH: Double);
begin
  FHandle.x := AX;
  FHandle.y := AY;
  FHandle.w := AW;
  FHandle.h := AH;
end;

procedure TBLRect.Reset;
begin
  FHandle.x := 0;
  FHandle.y := 0;
  FHandle.w := 0;
  FHandle.h := 0;
end;

{ TBLLine }

function BLLine: TBLLine; overload; inline;
begin
  Result.Reset;
end;

function BLLine(const AX0, AY0, AX1, AY1: Double): TBLLine; overload; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function BLLine(const AP0, AP1: TBLPoint): TBLLine; overload; inline;
begin
  Result.Reset(AP0, AP1);
end;

class operator TBLLine.Equal(const ALeft, ARight: TBLLine): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLLine.Equals(const AOther: TBLLine): Boolean;
begin
  Result := (FHandle.x0 = AOther.FHandle.x0)
        and (FHandle.y0 = AOther.FHandle.y0)
        and (FHandle.x1 = AOther.FHandle.x1)
        and (FHandle.y1 = AOther.FHandle.y1);
end;

class operator TBLLine.NotEqual(const ALeft, ARight: TBLLine): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLLine.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
end;

procedure TBLLine.Reset(const AP0, AP1: TBLPoint);
begin
  FHandle.x0 := AP0.FHandle.x;
  FHandle.y0 := AP0.FHandle.y;
  FHandle.x1 := AP1.FHandle.x;
  FHandle.y1 := AP1.FHandle.y;
end;

procedure TBLLine.Reset;
begin
  FHandle.x0 := 0;
  FHandle.y0 := 0;
  FHandle.x1 := 0;
  FHandle.y1 := 0;
end;

{ TBLTriangle }

function BLTriangle: TBLTriangle; overload; inline;
begin
  Result.Reset;
end;

function BLTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double): TBLTriangle; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1, AX2, AY2);
end;

class operator TBLTriangle.Equal(const ALeft, ARight: TBLTriangle): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLTriangle.Equals(const AOther: TBLTriangle): Boolean;
begin
  Result := (FHandle.x0 = AOther.FHandle.x0)
        and (FHandle.y0 = AOther.FHandle.y0)
        and (FHandle.x1 = AOther.FHandle.x1)
        and (FHandle.y1 = AOther.FHandle.y1)
        and (FHandle.x2 = AOther.FHandle.x2)
        and (FHandle.y2 = AOther.FHandle.y2);
end;

class operator TBLTriangle.NotEqual(const ALeft, ARight: TBLTriangle): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLTriangle.Reset(const AX0, AY0, AX1, AY1, AX2, AY2: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
  FHandle.x2 := AX2;
  FHandle.y2 := AY2;
end;

procedure TBLTriangle.Reset;
begin
  FHandle.x0 := 0;
  FHandle.y0 := 0;
  FHandle.x1 := 0;
  FHandle.y1 := 0;
  FHandle.x2 := 0;
  FHandle.y2 := 0;
end;

{ TBLRoundRect }

function BLRoundRect: TBLRoundRect; overload; inline;
begin
  Result.Reset;
end;

function BLRoundRect(const ARect: TBLRect; const AR: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(ARect, AR);
end;

function BLRoundRect(const ARect: TBLRect; const ARX, ARY: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(ARect, ARX, ARY);
end;

function BLRoundRect(const AX, AY, AW, AH, AR: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(AX, AY, AW, AH, AR);
end;

function BLRoundRect(const AX, AY, AW, AH, ARX, ARY: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(AX, AY, AW, AH, ARX, ARY);
end;

class operator TBLRoundRect.Equal(const ALeft, ARight: TBLRoundRect): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRoundRect.Equals(const AOther: TBLRoundRect): Boolean;
begin
  Result := (FHandle.x = AOther.FHandle.x)
        and (FHandle.y = AOther.FHandle.y)
        and (FHandle.w = AOther.FHandle.w)
        and (FHandle.h = AOther.FHandle.h)
        and (FHandle.rx = AOther.FHandle.rx)
        and (FHandle.ry = AOther.FHandle.ry);
end;

class operator TBLRoundRect.NotEqual(const ALeft,
  ARight: TBLRoundRect): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRoundRect.Reset(const ARect: TBLRect; const AR: Double);
begin
  FHandle.x := ARect.FHandle.x;
  FHandle.y := ARect.FHandle.y;
  FHandle.w := ARect.FHandle.w;
  FHandle.h := ARect.FHandle.h;
  FHandle.rx := AR;
  FHandle.ry := AR;
end;

procedure TBLRoundRect.Reset(const AX, AY, AW, AH, AR: Double);
begin
  FHandle.x := AX;
  FHandle.y := AY;
  FHandle.w := AW;
  FHandle.h := AH;
  FHandle.rx := AR;
  FHandle.ry := AR;
end;

procedure TBLRoundRect.Reset(const AX, AY, AW, AH, ARX, ARY: Double);
begin
  FHandle.x := AX;
  FHandle.y := AY;
  FHandle.w := AW;
  FHandle.h := AH;
  FHandle.rx := ARX;
  FHandle.ry := ARY;
end;

procedure TBLRoundRect.Reset(const ARect: TBLRect; const ARX, ARY: Double);
begin
  FHandle.x := ARect.FHandle.x;
  FHandle.y := ARect.FHandle.y;
  FHandle.w := ARect.FHandle.w;
  FHandle.h := ARect.FHandle.h;
  FHandle.rx := ARX;
  FHandle.ry := ARY;
end;

procedure TBLRoundRect.Reset;
begin
  FHandle.x := 0;
  FHandle.y := 0;
  FHandle.w := 0;
  FHandle.h := 0;
  FHandle.rx := 0;
  FHandle.ry := 0;
end;

{ TBLCircle }

function BLCircle: TBLCircle; overload; inline;
begin
  Result.Reset;
end;

function BLCircle(const ACX, ACY, AR: Double): TBLCircle; inline;
begin
  Result.Reset(ACX, ACY, AR);
end;

class operator TBLCircle.Equal(const ALeft, ARight: TBLCircle): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLCircle.Equals(const AOther: TBLCircle): Boolean;
begin
  Result := (FHandle.cx = AOther.FHandle.cx)
        and (FHandle.cy = AOther.FHandle.cy)
        and (FHandle.r = AOther.FHandle.r);
end;

class operator TBLCircle.NotEqual(const ALeft, ARight: TBLCircle): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLCircle.Reset(const ACX, ACY, AR: Double);
begin
  FHandle.cx := ACX;
  FHandle.cy := ACY;
  FHandle.r := AR;
end;

procedure TBLCircle.Reset;
begin
  FHandle.cx := 0;
  FHandle.cy := 0;
  FHandle.r := 0;
end;

{ TBLEllipse }

function BLEllipse: TBLEllipse; overload; inline;
begin
  Result.Reset;
end;

function BLEllipse(const ACX, ACY, ARX, ARY: Double): TBLEllipse; inline;
begin
  Result.Reset(ACX, ACY, ARX, ARY);
end;

class operator TBLEllipse.Equal(const ALeft, ARight: TBLEllipse): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLEllipse.Equals(const AOther: TBLEllipse): Boolean;
begin
  Result := (FHandle.cx = AOther.FHandle.cx)
        and (FHandle.cy = AOther.FHandle.cy)
        and (FHandle.rx = AOther.FHandle.rx)
        and (FHandle.ry = AOther.FHandle.ry);
end;

class operator TBLEllipse.NotEqual(const ALeft, ARight: TBLEllipse): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLEllipse.Reset(const ACX, ACY, ARX, ARY: Double);
begin
  FHandle.cx := ACX;
  FHandle.cy := ACY;
  FHandle.rx := ARX;
  FHandle.ry := ARY;
end;

procedure TBLEllipse.Reset;
begin
  FHandle.cx := 0;
  FHandle.cy := 0;
  FHandle.rx := 0;
  FHandle.ry := 0;
end;

{ TBLArc }

function BLArc: TBLArc; overload; inline;
begin
  Result.Reset;
end;

function BLArc(const ACX, ACY, AR, AStart, ASweep: Double): TBLArc; overload; inline;
begin
  Result.Reset(ACX, ACY, AR, AStart, ASweep);
end;

function BLArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double): TBLArc; overload; inline;
begin
  Result.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
end;

class operator TBLArc.Equal(const ALeft, ARight: TBLArc): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLArc.Equals(const AOther: TBLArc): Boolean;
begin
  Result := (FHandle.cx = AOther.FHandle.cx)
        and (FHandle.cy = AOther.FHandle.cy)
        and (FHandle.rx = AOther.FHandle.rx)
        and (FHandle.ry = AOther.FHandle.ry)
        and (FHandle.start = AOther.FHandle.start)
        and (FHandle.sweep = AOther.FHandle.sweep);
end;

class operator TBLArc.NotEqual(const ALeft, ARight: TBLArc): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLArc.Reset(const ACX, ACY, ARX, ARY, AStart, ASweep: Double);
begin
  FHandle.cx := ACX;
  FHandle.cy := ACY;
  FHandle.rx := ARX;
  FHandle.ry := ARY;
  FHandle.start := AStart;
  FHandle.sweep := ASweep;
end;

procedure TBLArc.Reset(const ACX, ACY, AR, AStart, ASweep: Double);
begin
  FHandle.cx := ACX;
  FHandle.cy := ACY;
  FHandle.rx := AR;
  FHandle.ry := AR;
  FHandle.start := AStart;
  FHandle.sweep := ASweep;
end;

procedure TBLArc.Reset;
begin
  FHandle.cx := 0;
  FHandle.cy := 0;
  FHandle.rx := 0;
  FHandle.ry := 0;
  FHandle.start := 0;
  FHandle.sweep := 0;
end;

{ Globals }

function BLAbs(const AA: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Abs(AA.FHandle.x), Abs(AA.FHandle.y));
end;

function BLAbs(const AA: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Abs(AA.FHandle.w), Abs(AA.FHandle.h));
end;

function BLMin(const AA, AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA.FHandle.x, AB.FHandle.x), Min(AA.FHandle.y, AB.FHandle.y));
end;

function BLMin(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA.FHandle.x, AB), Min(AA.FHandle.y, AB));
end;

function BLMin(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA, AB.FHandle.x), Min(AA, AB.FHandle.y));
end;

function BLMin(const AA, AB: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Min(AA.FHandle.w, AB.FHandle.w), Min(AA.FHandle.h, AB.FHandle.h));
end;

function BLMax(const AA, AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA.FHandle.x, AB.FHandle.x), Max(AA.FHandle.y, AB.FHandle.y));
end;

function BLMax(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA.FHandle.x, AB), Max(AA.FHandle.y, AB));
end;

function BLMax(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA, AB.FHandle.x), Max(AA, AB.FHandle.y));
end;

function BLMax(const AA, AB: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Max(AA.FHandle.w, AB.FHandle.w), Max(AA.FHandle.h, AB.FHandle.h));
end;

function BLClamp(const AA: TBLPoint; const AB, AC: Double): TBLPoint; inline;
begin
  Result := BLMin(AC, BLMax(AA, AB));
end;

{$ENDREGION 'Geometry'}

{$REGION 'Matrix'}

{ TBLMatrix2D }

function BLMatrix2D: TBLMatrix2D; overload; inline;
begin
  Result.Reset;
end;

function BLMatrix2D(const AM00, AM01, AM10, AM11, AM20, AM21: Double): TBLMatrix2D; overload; inline;
begin
  Result.Reset(AM00, AM01, AM10, AM11, AM20, AM21);
end;

class operator TBLMatrix2D.Equal(const ALeft, ARight: TBLMatrix2D): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLMatrix2D.Equals(const AOther: TBLMatrix2D): Boolean;
begin
  Result := (FHandle.m00 = AOther.FHandle.m00)
        and (FHandle.m01 = AOther.FHandle.m01)
        and (FHandle.m10 = AOther.FHandle.m10)
        and (FHandle.m11 = AOther.FHandle.m11)
        and (FHandle.m20 = AOther.FHandle.m20)
        and (FHandle.m21 = AOther.FHandle.m21);
end;

function TBLMatrix2D.GetDeterminant: Double;
begin
  Result := (FHandle.m00 * FHandle.m11) - (FHandle.m01 * FHandle.m10);
end;

function TBLMatrix2D.GetElement(const AIndex: TBLMatrix2DValue): Double;
begin
  Result := FHandle.m[Ord(AIndex)];
end;

function TBLMatrix2D.GetMatrixType: TBLMatrix2DType;
begin
  Result := TBLMatrix2DType(blMatrix2DGetType(@FHandle));
end;

class function TBLMatrix2D.Invert(const ASrc: TBLMatrix2D;
  out ADst: TBLMatrix2D): Boolean;
begin
  Result := (blMatrix2DInvert(@ADst.FHandle, @ASrc.FHandle) = BL_SUCCESS);
end;

function TBLMatrix2D.Invert: Boolean;
begin
  Result := (blMatrix2DInvert(@FHandle, @FHandle) = BL_SUCCESS);
end;

class function TBLMatrix2D.MakeIdentity: TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, 0, 0);
end;

class function TBLMatrix2D.MakeRotation(const AAngle, AX,
  AY: Double): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, AX, AY);
end;

class function TBLMatrix2D.MakeRotation(const AAngle: Double): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, 0, 0);
end;

class function TBLMatrix2D.MakeRotation(const AAngle: Double;
  const AP: TBLPoint): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, AP.FHandle.x, AP.FHandle.y);
end;

class function TBLMatrix2D.MakeScaling(const AX, AY: Double): TBLMatrix2D;
begin
  Result.Reset(AX, 0, 0, AY, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AP: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(AP.FHandle.x, 0, 0, AP.FHandle.y, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AP: TBLPointI): TBLMatrix2D;
begin
  Result.Reset(AP.FHandle.x, 0, 0, AP.FHandle.y, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AXY: Double): TBLMatrix2D;
begin
  Result.Reset(AXY, 0, 0, AXY, 0, 0);
end;

class function TBLMatrix2D.MakeSinCos(const ASin, ACos: Double;
  const ATranslate: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(ACos, ASin, -ASin, ACos, ATranslate.FHandle.x, ATranslate.FHandle.y);
end;

class function TBLMatrix2D.MakeSinCos(const ASin, ACos, ATranslateX,
  ATranslateY: Double): TBLMatrix2D;
begin
  Result.Reset(ACos, ASin, -ASin, ACos, ATranslateX, ATranslateY);
end;

class function TBLMatrix2D.MakeSkewing(const AX, AY: Double): TBLMatrix2D;
begin
  Result.ResetToSkewing(AX, AY);
end;

class function TBLMatrix2D.MakeSkewing(const AP: TBLPoint): TBLMatrix2D;
begin
  Result.ResetToSkewing(AP.FHandle.x, AP.FHandle.y);
end;

class function TBLMatrix2D.MakeTranslation(const AP: TBLPointI): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AP.FHandle.x, AP.FHandle.y);
end;

class function TBLMatrix2D.MakeTranslation(const AP: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AP.FHandle.x, AP.FHandle.y);
end;

class function TBLMatrix2D.MakeTranslation(const AX, AY: Double): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AX, AY);
end;

function TBLMatrix2D.MapPoint(const AP: TBLPoint): TBLPoint;
begin
  Result.Reset(
    (AP.FHandle.x * FHandle.m00) + (AP.FHandle.y * FHandle.m10) + FHandle.m20,
    (AP.FHandle.x * FHandle.m01) + (AP.FHandle.y * FHandle.m11) + FHandle.m21);
end;

function TBLMatrix2D.MapPoint(const AX, AY: Double): TBLPoint;
begin
  Result.Reset(
    (AX * FHandle.m00) + (AY * FHandle.m10) + FHandle.m20,
    (AX * FHandle.m01) + (AY * FHandle.m11) + FHandle.m21);
end;

function TBLMatrix2D.MapVector(const AX, AY: Double): TBLPoint;
begin
  Result.Reset(
    (AX * FHandle.m00) + (AY * FHandle.m10),
    (AX * FHandle.m01) + (AY * FHandle.m11));
end;

function TBLMatrix2D.MapVector(const AV: TBLPoint): TBLPoint;
begin
  Result.Reset(
    (AV.FHandle.x * FHandle.m00) + (AV.FHandle.y * FHandle.m10),
    (AV.FHandle.x * FHandle.m01) + (AV.FHandle.y * FHandle.m11));
end;

class operator TBLMatrix2D.NotEqual(const ALeft, ARight: TBLMatrix2D): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLMatrix2D.PostRotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLMatrix2D.PostRotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLMatrix2D.PostRotate(const AAngle: Double);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE, @AAngle));
end;

procedure TBLMatrix2D.PostScale(const AP: TBLPoint);
begin
  PostScale(AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.PostScale(const AX, AY: Double);
begin
  FHandle.m00 := FHandle.m00 * AX;
  FHandle.m01 := FHandle.m01 * AY;
  FHandle.m10 := FHandle.m10 * AX;
  FHandle.m11 := FHandle.m11 * AY;
  FHandle.m20 := FHandle.m10 * AX;
  FHandle.m21 := FHandle.m11 * AY;
end;

procedure TBLMatrix2D.PostScale(const AXY: Double);
begin
  PostScale(AXY, AXY);
end;

procedure TBLMatrix2D.PostScale(const AP: TBLPointI);
begin
  PostScale(AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.PostSkew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @Data));
end;

procedure TBLMatrix2D.PostSkew(const AP: TBLPoint);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @AP));
end;

procedure TBLMatrix2D.PostTransform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSFORM, @AMatrix));
end;

procedure TBLMatrix2D.PostTranslate(const AX, AY: Double);
begin
  FHandle.m20 := FHandle.m20 + AX;
  FHandle.m21 := FHandle.m21 + AY;
end;

procedure TBLMatrix2D.PostTranslate(const AP: TBLPoint);
begin
  FHandle.m20 := FHandle.m20 + AP.FHandle.x;
  FHandle.m21 := FHandle.m21 + AP.FHandle.y;
end;

procedure TBLMatrix2D.PostTranslate(const AP: TBLPointI);
begin
  FHandle.m20 := FHandle.m20 + AP.FHandle.x;
  FHandle.m21 := FHandle.m21 + AP.FHandle.y;
end;

procedure TBLMatrix2D.Reset(const AM00, AM01, AM10, AM11, AM20, AM21: Double);
begin
  FHandle.m00 := AM00;
  FHandle.m01 := AM01;
  FHandle.m10 := AM10;
  FHandle.m11 := AM11;
  FHandle.m20 := AM20;
  FHandle.m21 := AM21;
end;

procedure TBLMatrix2D.Reset;
begin
  FHandle.m00 := 1;
  FHandle.m01 := 0;
  FHandle.m10 := 0;
  FHandle.m11 := 1;
  FHandle.m20 := 0;
  FHandle.m21 := 0;
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle: Double);
begin
  _BLCheck(blMatrix2DSetRotation(@FHandle, AAngle, 0, 0));
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle, AX, AY: Double);
begin
  _BLCheck(blMatrix2DSetRotation(@FHandle, AAngle, AX, AY));
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle: Double; const AP: TBLPoint);
begin
  _BLCheck(blMatrix2DSetRotation(@FHandle, AAngle, AP.FHandle.x, AP.FHandle.y));
end;

procedure TBLMatrix2D.ResetToScaling(const AX, AY: Double);
begin
  Reset(AX, 0, 0, AY, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AP: TBLPoint);
begin
  Reset(AP.FHandle.x, 0, 0, AP.FHandle.y, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AXY: Double);
begin
  Reset(AXY, 0, 0, AXY, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AP: TBLPointI);
begin
  Reset(AP.FHandle.x, 0, 0, AP.FHandle.y, 0, 0);
end;

procedure TBLMatrix2D.ResetToSinCos(const ASin, ACos, ATranslateX,
  ATranslateY: Double);
begin
  Reset(ACos, ASin, -ASin, ACos, ATranslateX, ATranslateY);
end;

procedure TBLMatrix2D.ResetToSinCos(const ASin, ACos: Double;
  const ATranslate: TBLPoint);
begin
  Reset(ACos, ASin, -ASin, ACos, ATranslate.FHandle.x, ATranslate.FHandle.y);
end;

procedure TBLMatrix2D.ResetToSkewing(const AP: TBLPoint);
begin
  _BLCheck(blMatrix2DSetSkewing(@FHandle, AP.FHandle.x, AP.FHandle.y));
end;

procedure TBLMatrix2D.ResetToSkewing(const AX, AY: Double);
begin
  _BLCheck(blMatrix2DSetSkewing(@FHandle, AX, AY));
end;

procedure TBLMatrix2D.ResetToTranslation(const AP: TBLPointI);
begin
  Reset(1, 0, 0, 1, AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.ResetToTranslation(const AP: TBLPoint);
begin
  Reset(1, 0, 0, 1, AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.ResetToTranslation(const AX, AY: Double);
begin
  Reset(1, 0, 0, 1, AX, AY);
end;

procedure TBLMatrix2D.Rotate(const AAngle: Double);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_ROTATE, @AAngle));
end;

procedure TBLMatrix2D.Rotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLMatrix2D.Rotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLMatrix2D.Scale(const AXY: Double);
begin
  FHandle.m00 := FHandle.m00 * AXY;
  FHandle.m01 := FHandle.m01 * AXY;
  FHandle.m10 := FHandle.m10 * AXY;
  FHandle.m11 := FHandle.m11 * AXY;
end;

procedure TBLMatrix2D.Scale(const AP: TBLPointI);
begin
  Scale(AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.Scale(const AP: TBLPoint);
begin
  Scale(AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.Scale(const AX, AY: Double);
begin
  FHandle.m00 := FHandle.m00 * AX;
  FHandle.m01 := FHandle.m01 * AX;
  FHandle.m10 := FHandle.m10 * AY;
  FHandle.m11 := FHandle.m11 * AY;
end;

procedure TBLMatrix2D.SetElement(const AIndex: TBLMatrix2DValue;
  const AValue: Double);
begin
  FHandle.m[Ord(AIndex)] := AValue;
end;

procedure TBLMatrix2D.Skew(const AP: TBLPoint);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_SKEW, @AP));
end;

procedure TBLMatrix2D.Skew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_SKEW, @Data));
end;

procedure TBLMatrix2D.Transform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blMatrix2DApplyOp(@FHandle, BL_MATRIX2D_OP_TRANSFORM, @AMatrix));
end;

procedure TBLMatrix2D.Translate(const AX, AY: Double);
begin
  FHandle.m20 := FHandle.m20 + (AX * FHandle.m00) + (AY * FHandle.m10);
  FHandle.m21 := FHandle.m21 + (AX * FHandle.m01) + (AY * FHandle.m11);
end;

procedure TBLMatrix2D.Translate(const AP: TBLPoint);
begin
  Translate(AP.FHandle.x, AP.FHandle.y);
end;

procedure TBLMatrix2D.Translate(const AP: TBLPointI);
begin
  Translate(AP.FHandle.x, AP.FHandle.y);
end;

{$ENDREGION 'Matrix'}

{$REGION 'Random'}

{ TBLRandom }

class operator TBLRandom.Equal(const ALeft, ARight: TBLRandom): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRandom.Equals(const AOther: TBLRandom): Boolean;
begin
  Result := (FHandle.data[0] = AOther.FHandle.data[0])
        and (FHandle.data[1] = AOther.FHandle.data[1]);
end;

function TBLRandom.NextDouble: Double;
begin
  Result := blRandomNextDouble(@FHandle);
end;

function TBLRandom.NextUInt32: UInt32;
begin
  Result := blRandomNextUInt32(@FHandle);
end;

function TBLRandom.NextUInt64: UInt64;
begin
  Result := blRandomNextUInt64(@FHandle);
end;

class operator TBLRandom.NotEqual(const ALeft, ARight: TBLRandom): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRandom.Reset(const ASeed: UInt64);
begin
  blRandomReset(@FHandle, ASeed);
end;

{$ENDREGION 'Random'}

{$REGION 'Gradient'}

{ TBLGradientStop }

function BLGradientStop(const AOffset: Double; const ARgba: TBLRgba32): TBLGradientStop; overload; inline;
begin
  Result.Reset(AOffset, ARgba);
end;

function BLGradientStop(const AOffset: Double; const ARgba: TBLRgba64): TBLGradientStop; overload; inline;
begin
  Result.Reset(AOffset, ARgba);
end;

class operator TBLGradientStop.Equal(const ALeft,
  ARight: TBLGradientStop): Boolean;
begin
  Result := (ALeft.FHandle.offset = ARight.FHandle.offset)
        and (ALeft.FHandle.rgba.value = ARight.FHandle.rgba.value);
end;

function TBLGradientStop.Equals(const AOther: TBLGradientStop): Boolean;
begin
  Result := (Self = AOther);
end;

function TBLGradientStop.GetRgba: TBLRgba64;
begin
  Result.FHandle.value := FHandle.rgba.value;
end;

class operator TBLGradientStop.NotEqual(const ALeft,
  ARight: TBLGradientStop): Boolean;
begin
  Result := not (ALeft = ARight);
end;

procedure TBLGradientStop.Reset(const AOffset: Double; const ARgba: TBLRgba64);
begin
  FHandle.offset := AOffset;
  FHandle.rgba.value := ARgba.FHandle.value;
end;

procedure TBLGradientStop.Reset(const AOffset: Double; const ARgba: TBLRgba32);
begin
  FHandle.offset := AOffset;
  TBLRgba64(FHandle.rgba).Reset(ARgba);
end;

procedure TBLGradientStop.Reset;
begin
  FHandle.offset := 0;
  FHandle.rgba.value := 0;
end;

procedure TBLGradientStop.SetRgba(const AValue: TBLRgba64);
begin
  FHandle.rgba.value := AValue.FHandle.value;
end;

{ TBLLinearGradientValues }

function BLLinearGradientValues(const AX0, AY0, AX1, AY1: Double): TBLLinearGradientValues; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

procedure TBLLinearGradientValues.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
end;

procedure TBLLinearGradientValues.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLRadialGradientValues }

function BLRadialGradientValues(const AX0, AY0, AX1, AY1, AR0: Double): TBLRadialGradientValues; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1, AR0);
end;

procedure TBLRadialGradientValues.Reset(const AX0, AY0, AX1, AY1, AR0: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.x1 := AX1;
  FHandle.y1 := AY1;
  FHandle.r0 := AR0;
end;

procedure TBLRadialGradientValues.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLConicalGradientValues }

function BLConicalGradientValues(const AX0, AY0, AAngle: Double): TBLConicalGradientValues; inline;
begin
  Result.Reset(AX0, AY0, AAngle);
end;

procedure TBLConicalGradientValues.Reset(const AX0, AY0, AAngle: Double);
begin
  FHandle.x0 := AX0;
  FHandle.y0 := AY0;
  FHandle.angle := AAngle;
end;

procedure TBLConicalGradientValues.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLGradient }

procedure TBLGradient.AddStop(const AOffset: Double; const ARgba: TBLRgba64);
begin
  _BLCheck(blGradientAddStopRgba64(@FHandle, AOffset, ARgba.FHandle.value));
end;

procedure TBLGradient.AddStop(const AOffset: Double; const ARgba: TBLRgba32);
begin
  _BLCheck(blGradientAddStopRgba32(@FHandle, AOffset, ARgba.FHandle.value));
end;

constructor TBLGradient.Create;
begin
  inherited;
  blGradientInit(@FHandle);
end;

constructor TBLGradient.Create(const AValues: TBLRadialGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_RADIAL, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

constructor TBLGradient.Create(const AValues: TBLLinearGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_LINEAR, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

constructor TBLGradient.Create(const AType: TBLGradientType;
  const AValues: PDouble);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, Ord(AType), AValues, BL_EXTEND_MODE_PAD,
    nil, 0, nil));
end;

constructor TBLGradient.Create(const AValues: TBLConicalGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_CONICAL, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

constructor TBLGradient.Create(const AValues: TBLConicalGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_CONICAL, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

constructor TBLGradient.Create(const AHandle: BLGradientCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
end;

constructor TBLGradient.Create(const AValues: TBLRadialGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_RADIAL, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

constructor TBLGradient.Create(const AValues: TBLLinearGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  inherited Create;
  _BLCheck(blGradientInitAs(@FHandle, BL_GRADIENT_TYPE_LINEAR, @AValues.FHandle,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

destructor TBLGradient.Destroy;
begin
  if (not FIsReference) then
    blGradientDestroy(@FHandle);
  inherited;
end;

function TBLGradient.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLGradient) then
    Result := blGradientEquals(@FHandle, @TBLGradient(Obj).FHandle)
  else
    Result := False;
end;

function TBLGradient.Equals(const AOther: IBLGradient): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blGradientEquals(@FHandle, AOther.Handle);
end;

function TBLGradient.GetAllStops: PBLGradientStop;
begin
  Result := PBLGradientStop(FHandle.impl.stops.stops);
end;

function TBLGradient.GetAngle: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_CONICAL_ANGLE];
end;

function TBLGradient.GetCapacity: Integer;
begin
  Result := FHandle.impl.capacity;
end;

function TBLGradient.GetConical: TBLConicalGradientValues;
begin
  Result.FHandle := FHandle.impl.conical;
end;

function TBLGradient.GetExtendMode: TBLExtendMode;
begin
  Result := TBLExtendMode(FHandle.impl.extendMode);
end;

function TBLGradient.GetGradientType: TBLGradientType;
begin
  Result := TBLGradientType(FHandle.impl.gradientType);
end;

function TBLGradient.GetHandle: PBLGradientCore;
begin
  Result := @FHandle;
end;

function TBLGradient.GetHasMatrix: Boolean;
begin
  Result := ((FHandle.impl.matrixType and BL_MATRIX2D_TYPE_IDENTITY) <> 0);
end;

function TBLGradient.GetIsEmpty: Boolean;
begin
  Result := (FHandle.impl.stops.size = 0);
end;

function TBLGradient.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLGradient.GetLinear: TBLLinearGradientValues;
begin
  Result.FHandle := FHandle.impl.linear;
end;

function TBLGradient.GetMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.impl.matrix;
end;

function TBLGradient.GetMatrixType: TBLMatrix2DType;
begin
  Result := TBLMatrix2DType(FHandle.impl.matrixType);
end;

function TBLGradient.GetR0: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_RADIAL_R0];
end;

function TBLGradient.GetRadial: TBLRadialGradientValues;
begin
  Result.FHandle := FHandle.impl.radial;
end;

function TBLGradient.GetSize: Integer;
begin
  Result := FHandle.impl.stops.size;
end;

function TBLGradient.GetStop(const AIndex: Integer): TBLGradientStop;
begin
  Assert(Cardinal(AIndex) < FHandle.impl.stops.size);
  Result := TBLGradientStop(FHandle.impl.stops.stops[AIndex]);
end;

function TBLGradient.GetValue(const AIndex: TBLGradientValue): Double;
begin
  Result := FHandle.impl.values[Ord(AIndex)];
end;

function TBLGradient.GetX0: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_COMMON_X0];
end;

function TBLGradient.GetX1: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_COMMON_X1];
end;

function TBLGradient.GetY0: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_COMMON_Y0];
end;

function TBLGradient.GetY1: Double;
begin
  Result := FHandle.impl.values[BL_GRADIENT_VALUE_COMMON_Y1];
end;

function TBLGradient.IndexOfStop(const AOffset: Double): Integer;
begin
  Result := blGradientIndexOfStop(@FHandle, AOffset);
end;

procedure TBLGradient.Initialize(const AValues: TBLConicalGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_CONICAL, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

procedure TBLGradient.Initialize(const AValues: TBLConicalGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_CONICAL, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

procedure TBLGradient.Initialize(const AValues: TBLRadialGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_RADIAL, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

procedure TBLGradient.Initialize(const AValues: TBLLinearGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_LINEAR, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), nil));
end;

procedure TBLGradient.Initialize(const AValues: TBLRadialGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_RADIAL, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

procedure TBLGradient.Initialize(const AValues: TBLLinearGradientValues;
  const AExtendMode: TBLExtendMode; const AStops: TArray<TBLGradientStop>;
  const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blGradientCreate(@FHandle, BL_GRADIENT_TYPE_LINEAR, @AValues,
    Ord(AExtendMode), Pointer(AStops), Length(AStops), @AMatrix));
end;

procedure TBLGradient.PostRotate(const AAngle: Double);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE, @AAngle));
end;

procedure TBLGradient.PostRotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLGradient.PostRotate(const AAngle: Double; const AP: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLGradient.PostRotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLGradient.PostScale(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @AP));
end;

procedure TBLGradient.PostScale(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLGradient.PostScale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLGradient.PostScale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLGradient.PostSkew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @Data));
end;

procedure TBLGradient.PostSkew(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @AP));
end;

procedure TBLGradient.PostTransform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSFORM, @AMatrix));
end;

procedure TBLGradient.PostTranslate(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @AP));
end;

procedure TBLGradient.PostTranslate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLGradient.PostTranslate(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLGradient.RemoveStop(const AIndex: Integer);
begin
  _BLCheck(blGradientRemoveStop(@FHandle, AIndex));
end;

procedure TBLGradient.RemoveStopByOffset(const AOffset: Double;
  const AAll: Boolean);
begin
  _BLCheck(blGradientRemoveStopByOffset(@FHandle, AOffset, Ord(AAll)));
end;

procedure TBLGradient.RemoveStops(const ARange: TBLRange);
begin
  _BLCheck(blGradientRemoveStops(@FHandle, ARange.FHandle.start, ARange.FHandle.&end));
end;

procedure TBLGradient.RemoveStopsByOffset(const AOffsetMin, AOffsetMax: Double);
begin
  _BLCheck(blGradientRemoveStopsFromTo(@FHandle, AOffsetMin, AOffsetMax));
end;

procedure TBLGradient.ReplaceStop(const AIndex: Integer; const AOffset: Double;
  const ARgba: TBLRgba64);
begin
  _BLCheck(blGradientReplaceStopRgba64(@FHandle, AIndex, AOffset, ARgba.FHandle.value));
end;

procedure TBLGradient.ReplaceStop(const AIndex: Integer; const AOffset: Double;
  const ARgba: TBLRgba32);
begin
  _BLCheck(blGradientReplaceStopRgba32(@FHandle, AIndex, AOffset, ARgba.FHandle.value));
end;

procedure TBLGradient.Reserve(const ACount: Integer);
begin
  _BLCheck(blGradientReserve(@FHandle, ACount));
end;

procedure TBLGradient.Reset;
begin
  _BLCheck(blGradientReset(@FHandle));
end;

procedure TBLGradient.ResetExtendMode;
begin
  _BLCheck(blGradientSetExtendMode(@FHandle, BL_EXTEND_MODE_PAD));
end;

procedure TBLGradient.ResetMatrix;
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_RESET, nil));
end;

procedure TBLGradient.ResetStops;
begin
  _BLCheck(blGradientResetStops(@FHandle));
end;

procedure TBLGradient.Rotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLGradient.Rotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLGradient.Rotate(const AAngle: Double);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE, @AAngle));
end;

procedure TBLGradient.Rotate(const AAngle: Double; const AP: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLGradient.Scale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLGradient.Scale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLGradient.Scale(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLGradient.Scale(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @AP));
end;

procedure TBLGradient.SetAngle(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_CONICAL_ANGLE, AValue));
end;

procedure TBLGradient.SetConical(const AValue: TBLConicalGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValue, SizeOf(AValue) div SizeOf(Double)));
end;

procedure TBLGradient.SetExtendMode(const AValue: TBLExtendMode);
begin
  _BLCheck(blGradientSetExtendMode(@FHandle, Ord(AValue)));
end;

procedure TBLGradient.SetGradientType(const AValue: TBLGradientType);
begin
  _BLCheck(blGradientSetType(@FHandle, Ord(AValue)));
end;

procedure TBLGradient.SetLinear(const AValue: TBLLinearGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValue, SizeOf(AValue) div SizeOf(Double)));
end;

procedure TBLGradient.SetMatrix(const AValue: TBLMatrix2D);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ASSIGN, @AValue));
end;

procedure TBLGradient.SetR0(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_RADIAL_R0, AValue));
end;

procedure TBLGradient.SetRadial(const AValue: TBLRadialGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValue, SizeOf(AValue) div SizeOf(Double)));
end;

procedure TBLGradient.SetStop(const AIndex: Integer;
  const AValue: TBLGradientStop);
begin
  _BLCheck(blGradientReplaceStopRgba64(@FHandle, AIndex, AValue.FHandle.offset,
    AValue.FHandle.rgba.value));
end;

procedure TBLGradient.SetStops(const AStops: TArray<TBLGradientStop>);
begin
  _BLCheck(blGradientAssignStops(@FHandle, Pointer(AStops), Length(AStops)));
end;

procedure TBLGradient.SetValue(const AIndex: TBLGradientValue;
  const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, Ord(AIndex), AValue));
end;

procedure TBLGradient.SetValues(const AValues: TBLLinearGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValues, SizeOf(TBLLinearGradientValues) div SizeOf(Double)));
end;

procedure TBLGradient.SetValues(const AValues: TBLRadialGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValues, SizeOf(TBLRadialGradientValues) div SizeOf(Double)));
end;

procedure TBLGradient.SetValues(const AValues: TBLConicalGradientValues);
begin
  _BLCheck(blGradientSetValues(@FHandle, 0, @AValues, SizeOf(TBLConicalGradientValues) div SizeOf(Double)));
end;

procedure TBLGradient.SetValues(const AIndex: TBLGradientValue;
  const AValues: TArray<Double>);
begin
  _BLCheck(blGradientSetValues(@FHandle, Ord(AIndex), Pointer(AValues), Length(AValues)));
end;

procedure TBLGradient.SetX0(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_COMMON_X0, AValue));
end;

procedure TBLGradient.SetX1(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_COMMON_X1, AValue));
end;

procedure TBLGradient.SetY0(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_COMMON_Y0, AValue));
end;

procedure TBLGradient.SetY1(const AValue: Double);
begin
  _BLCheck(blGradientSetValue(@FHandle, BL_GRADIENT_VALUE_COMMON_Y1, AValue));
end;

procedure TBLGradient.Shrink;
begin
  _BLCheck(blGradientShrink(@FHandle));
end;

procedure TBLGradient.Skew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @Data));
end;

procedure TBLGradient.Skew(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @AP));
end;

procedure TBLGradient.Transform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSFORM, @AMatrix));
end;

procedure TBLGradient.Translate(const AP: TBLPoint);
begin
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @AP));
end;

procedure TBLGradient.Translate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

procedure TBLGradient.Translate(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blGradientApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

{$ENDREGION 'Gradient'}

{$REGION 'Region'}

{ TBLRegion }

procedure TBLRegion.Assign(const ABoxes: PBLBoxI; const ACount: Integer);
begin
  _BLCheck(blRegionAssignBoxIArray(@FHandle, Pointer(ABoxes), ACount));
end;

procedure TBLRegion.Assign(const ABoxes: TArray<TBLBoxI>);
begin
  _BLCheck(blRegionAssignBoxIArray(@FHandle, Pointer(ABoxes), Length(ABoxes)));
end;

procedure TBLRegion.Assign(const ABox: TBLBoxI);
begin
  _BLCheck(blRegionAssignBoxI(@FHandle, @ABox));
end;

procedure TBLRegion.Assign(const ARects: PBLRectI; const ACount: Integer);
begin
  _BLCheck(blRegionAssignRectIArray(@FHandle, Pointer(ARects), ACount));
end;

procedure TBLRegion.Assign(const ARect: TBLRectI);
begin
  _BLCheck(blRegionAssignRectI(@FHandle, @ARect));
end;

procedure TBLRegion.Assign(const ARects: TArray<TBLRectI>);
begin
  _BLCheck(blRegionAssignRectIArray(@FHandle, Pointer(ARects), Length(ARects)));
end;

procedure TBLRegion.Clear;
begin
  _BLCheck(blRegionClear(@FHandle));
end;

function TBLRegion.Clone: IBLRegion;
begin
  Result := TBLRegion.Create;
  _BLCheck(blRegionAssignDeep(Result.Handle, @FHandle));
end;

class function TBLRegion.Combine(const AA, AB: IBLRegion;
  const ABooleanOp: TBLBooleanOp): IBLRegion;
begin
  Result := TBLRegion.Create;
  if (AA <> nil) and (AB <> nil) then
    _BLCheck(blRegionCombine(Result.Handle, AA.Handle, AB.Handle, Ord(ABooleanOp)));
end;

procedure TBLRegion.Combine(const ABox: TBLBoxI;
  const ABooleanOp: TBLBooleanOp);
begin
  _BLCheck(blRegionCombineRB(@FHandle, @FHandle, @ABox, Ord(ABooleanOp)));
end;

procedure TBLRegion.Combine(const ARegion: IBLRegion;
  const ABooleanOp: TBLBooleanOp);
begin
  if (ARegion <> nil) then
    _BLCheck(blRegionCombine(@FHandle, @FHandle, ARegion.Handle, Ord(ABooleanOp)));
end;

class function TBLRegion.Combine(const AA, AB: TBLBoxI;
  const ABooleanOp: TBLBooleanOp): IBLRegion;
begin
  Result := TBLRegion.Create;
  _BLCheck(blRegionCombineBB(Result.Handle, @AA, @AB, Ord(ABooleanOp)));
end;

class function TBLRegion.Combine(const AA: TBLBoxI; const AB: IBLRegion;
  const ABooleanOp: TBLBooleanOp): IBLRegion;
begin
  Result := TBLRegion.Create;
  if (AB <> nil) then
    _BLCheck(blRegionCombineBR(Result.Handle, @AA, AB.Handle, Ord(ABooleanOp)));
end;

class function TBLRegion.Combine(const AA: IBLRegion; const AB: TBLBoxI;
  const ABooleanOp: TBLBooleanOp): IBLRegion;
begin
  Result := TBLRegion.Create;
  if (AA <> nil) then
    _BLCheck(blRegionCombineRB(Result.Handle, AA.Handle, @AB, Ord(ABooleanOp)));
end;

constructor TBLRegion.Create;
begin
  inherited;
  blRegionInit(@FHandle);
end;

destructor TBLRegion.Destroy;
begin
  blRegionDestroy(@FHandle);
  inherited;
end;

function TBLRegion.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLRegion) then
    Result := blRegionEquals(@FHandle, @TBLRegion(Obj).FHandle)
  else
    Result := False;
end;

function TBLRegion.Equals(const AOther: IBLRegion): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blRegionEquals(@FHandle, AOther.Handle);
end;

function TBLRegion.GetBoundingBox: TBLBoxI;
begin
  Result.FHandle := FHandle.impl.boundingBox;
end;

function TBLRegion.GetCapacity: Integer;
begin
  Result := FHandle.impl.capacity;
end;

function TBLRegion.GetData: PBLBoxI;
begin
  Result := Pointer(FHandle.impl.data.data);
end;

function TBLRegion.GetDataEnd: PBLBoxI;
begin
  Result := Pointer(FHandle.impl.data.data);
  Inc(Result, FHandle.impl.data.size);
end;

function TBLRegion.GetHandle: PBLRegionCore;
begin
  Result := @FHandle;
end;

function TBLRegion.GetIsComplex: Boolean;
begin
  Result := (FHandle.impl.data.size > 1);
end;

function TBLRegion.GetIsEmpty: Boolean;
begin
  Result := (FHandle.impl.data.size = 0);
end;

function TBLRegion.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLRegion.GetIsRect: Boolean;
begin
  Result := (FHandle.impl.data.size = 1);
end;

function TBLRegion.GetRegionType: TBLRegionType;
begin
  Result := TBLRegionType(Min(FHandle.impl.data.size, BL_REGION_TYPE_COMPLEX));
end;

function TBLRegion.GetSize: Integer;
begin
  Result := FHandle.impl.data.size;
end;

function TBLRegion.GetView: TBLRegionView;
begin
  Result.FHandle := BLArrayView(FHandle.impl.data.view);
end;

function TBLRegion.HitTest(const ABox: TBLBoxI): TBLHitTest;
begin
  Result := TBLHitTest(blRegionHitTestBoxI(@FHandle, @ABox));
end;

function TBLRegion.HitTest(const APt: TBLPointI): TBLHitTest;
begin
  Result := TBLHitTest(blRegionHitTest(@FHandle, @APt));
end;

class function TBLRegion.IntersectAndClip(const AA, AB: IBLRegion;
  const AClipBox: TBLBoxI): IBLRegion;
begin
  Result := TBLRegion.Create;
  if Assigned(AA) and Assigned(AB) then
    _BLCheck(blRegionIntersectAndClip(Result.Handle, AA.Handle, AB.Handle, @AClipBox));
end;

procedure TBLRegion.IntersectAndClip(const AR: IBLRegion;
  const AClipBox: TBLBoxI);
begin
  if Assigned(AR) then
    _BLCheck(blRegionIntersectAndClip(@FHandle, @FHandle, AR.Handle, @AClipBox));
end;

procedure TBLRegion.Reserve(const ACount: Integer);
begin
  _BLCheck(blRegionReserve(@FHandle, ACount));
end;

procedure TBLRegion.Reset;
begin
  _BLCheck(blRegionReset(@FHandle));
end;

procedure TBLRegion.Shrink;
begin
  _BLCheck(blRegionShrink(@FHandle));
end;

procedure TBLRegion.Translate(const APt: TBLPointI);
begin
  _BLCheck(blRegionTranslate(@FHandle, @FHandle, @APt));
end;

class function TBLRegion.Translate(const AR: IBLRegion;
  const APt: TBLPointI): IBLRegion;
begin
  Result := TBLRegion.Create;
  if Assigned(AR) then
    _BLCheck(blRegionTranslate(Result.Handle, AR.Handle, @APt));
end;

class function TBLRegion.TranslateAndClip(const AR: IBLRegion;
  const APt: TBLPointI; const AClipBox: TBLBoxI): IBLRegion;
begin
  Result := TBLRegion.Create;
  if Assigned(AR) then
    _BLCheck(blRegionTranslateAndClip(Result.Handle, AR.Handle, @APt, @AClipBox));
end;

procedure TBLRegion.TranslateAndClip(const APt: TBLPointI;
  const AClipBox: TBLBoxI);
begin
  _BLCheck(blRegionTranslateAndClip(@FHandle, @FHandle, @APt, @AClipBox));
end;

{$ENDREGION 'Region'}

{$REGION 'Path'}

{ TBLApproximationOptions }

class constructor TBLApproximationOptions.Create;
begin
  FillChar(FDefault, SizeOf(FDefault), 0);
  FDefault.flattenTolerance := 0.2;
  FDefault.simplifyTolerance := 0.05;
  FDefault.offsetParameter := 0.414213562;
end;

class function TBLApproximationOptions.GetDefault: TBLApproximationOptions;
begin
  Result.FHandle := FDefault;
end;

function TBLApproximationOptions.GetFlattenMode: TBLFlattenMode;
begin
  Result := TBLFlattenMode(FHandle.flattenMode);
end;

function TBLApproximationOptions.GetOffsetMode: TBLOffsetMode;
begin
  Result := TBLOffsetMode(FHandle.offsetMode);
end;

procedure TBLApproximationOptions.SetFlattenMode(const AValue: TBLFlattenMode);
begin
  FHandle.flattenMode := Ord(AValue);
end;

procedure TBLApproximationOptions.SetOffsetMode(const AValue: TBLOffsetMode);
begin
  FHandle.offsetMode := Ord(AValue);
end;

{ TBLStrokeOptions }

function TBLStrokeOptions.GetDashArray: TArray<Double>;
begin
  Result := TBLUtils.BLArrayToArray<Double>(FHandle.dashArray);
end;

function TBLStrokeOptions.GetEndCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FHandle.options.endCap);
end;

function TBLStrokeOptions.GetJoin: TBLStrokeJoin;
begin
  Result := TBLStrokeJoin(FHandle.options.join);
end;

function TBLStrokeOptions.GetStartCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FHandle.options.startCap);
end;

function TBLStrokeOptions.GetTransformOrder: TBLStrokeTransformOrder;
begin
  Result := TBLStrokeTransformOrder(FHandle.options.transformOrder);
end;

procedure TBLStrokeOptions.Reset;
begin
  if (FScope <> nil) then
    blStrokeOptionsReset(@FHandle)
  else
  begin
    blStrokeOptionsInit(@FHandle);
    FScope := TScope.Create(@FHandle);
  end;
end;

procedure TBLStrokeOptions.SetCaps(const AStartCap, AEndCap: TBLStrokeCap);
begin
  FHandle.options.startCap := Ord(AStartCap);
  FHandle.options.endCap := Ord(AEndCap);
end;

procedure TBLStrokeOptions.SetCaps(const ACap: TBLStrokeCap);
begin
  FHandle.options.startCap := Ord(ACap);
  FHandle.options.endCap := Ord(ACap);
end;

procedure TBLStrokeOptions.SetDashArray(const AValue: TArray<Double>);
var
  Value: IBLArray;
begin
  blArrayReset(@FHandle.dashArray);
  Value := TBLUtils.ArrayToBLArray<Double>(AValue);
  Value.RevokeOwnership;
  FHandle.dashArray := Value.Handle^;
end;

procedure TBLStrokeOptions.SetEndCap(const AValue: TBLStrokeCap);
begin
  FHandle.options.endCap := Ord(AValue);
end;

procedure TBLStrokeOptions.SetJoin(const AValue: TBLStrokeJoin);
begin
  FHandle.options.join := Ord(AValue);
end;

procedure TBLStrokeOptions.SetStartCap(const AValue: TBLStrokeCap);
begin
  FHandle.options.startCap := Ord(AValue);
end;

procedure TBLStrokeOptions.SetTransformOrder(
  const AValue: TBLStrokeTransformOrder);
begin
  FHandle.options.transformOrder := Ord(AValue);
end;

{ TBLStrokeOptions.TScope }

constructor TBLStrokeOptions.TScope.Create(const AHandle: PBLStrokeOptionsCore);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TBLStrokeOptions.TScope.Destroy;
begin
  blStrokeOptionsDestroy(FHandle);
  inherited;
end;

{ TBLPathView }

function BLPathView(const ACommands: PBLPathCmd; const AVertices: PBLPoint;
  const ACount: Integer): TBLPathView; inline;
begin
  Result.Reset(ACommands, AVertices, ACount);
end;

function TBLPathView.GetCommands: PBLPathCmd;
begin
  Result := Pointer(FHandle.commandData);
end;

function TBLPathView.GetCount: Integer;
begin
  Result := FHandle.size;
end;

function TBLPathView.GetVertices: PBLPoint;
begin
  Result := Pointer(FHandle.vertexData);
end;

procedure TBLPathView.Reset(const ACommands: PBLPathCmd;
  const AVertices: PBLPoint; const ACount: Integer);
begin
  FHandle.commandData := Pointer(ACommands);
  FHandle.vertexData := Pointer(AVertices);
  FHandle.size := ACount;
end;

procedure TBLPathView.Reset;
begin
  FHandle.commandData := nil;
  FHandle.vertexData := nil;
  FHandle.size := 0;
end;

{ TBLPath }

procedure TBLPath.AddArc(const AArc: TBLArc;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARC, @AArc, nil, Ord(ADirection)));
end;

procedure TBLPath.AddArc(const AArc: TBLArc; const AMatrix: TBLMatrix2D;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARC, @AArc, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddBox(const ABox: TBLBoxI;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddBoxI(@FHandle, @ABox, Ord(ADirection)));
end;

procedure TBLPath.AddBox(const ABox: TBLBox;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddBoxD(@FHandle, @ABox, Ord(ADirection)));
end;

procedure TBLPath.AddBox(const AX0, AY0, AX1, AY1: Double;
  const ADirection: TBLGeometryDirection);
var
  Box: TBLBox;
begin
  Box.Reset(AX0, AY0, AX1, AY1);
  _BLCheck(blPathAddBoxD(@FHandle, @Box, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @ABoxes.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: TBLArrayView<TBLBoxI>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @ABoxes.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: PBLBoxI; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: PBLBox; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @ABoxes.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddBoxArray(const ABoxes: TBLArrayView<TBLBox>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @ABoxes.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddChord(const AChord: TBLArc;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @AChord, nil, Ord(ADirection)));
end;

procedure TBLPath.AddChord(const AChord: TBLArc; const AMatrix: TBLMatrix2D;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @AChord, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddCircle(const ACircle: TBLCircle;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @ACircle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddCircle(const ACircle: TBLCircle;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @ACircle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddEllipse(const AEllipse: TBLEllipse;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @AEllipse, nil, Ord(ADirection)));
end;

procedure TBLPath.AddEllipse(const AEllipse: TBLEllipse;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @AEllipse, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddGeometry(const AGeometryType: TBLGeometryType;
  const AGeometryData: Pointer; const AMatrix: PBLMatrix2D;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, Ord(AGeometryType), AGeometryData,
    @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddLine(const ALine: TBLLine; const AMatrix: TBLMatrix2D;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_LINE, @ALine, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddLine(const ALine: TBLLine;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_LINE, @ALine, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPath(const APath: IBLPath; const ARange: TBLRange);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddPath(@FHandle, APath.Handle, @ARange));
end;

procedure TBLPath.AddPath(const APath: IBLPath);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddPath(@FHandle, APath.Handle, nil));
end;

procedure TBLPath.AddPath(const APath: IBLPath; const ATranslate: TBLPoint);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddTranslatedPath(@FHandle, APath.Handle, nil, @ATranslate));
end;

procedure TBLPath.AddPath(const APath: IBLPath; const ARange: TBLRange;
  const AMatrix: TBLMatrix2D);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddTransformedPath(@FHandle, APath.Handle, @ARange, @AMatrix));
end;

procedure TBLPath.AddPath(const APath: IBLPath; const AMatrix: TBLMatrix2D);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddTransformedPath(@FHandle, APath.Handle, nil, @AMatrix));
end;

procedure TBLPath.AddPath(const APath: IBLPath; const ARange: TBLRange;
  const ATranslate: TBLPoint);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddTranslatedPath(@FHandle, APath.Handle, @ARange, @ATranslate));
end;

procedure TBLPath.AddPie(const APie: TBLArc; const AMatrix: TBLMatrix2D;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @APie, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPie(const APie: TBLArc;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @APie, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @APolygon.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: TBLArrayView<TBLPoint>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @APolygon.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APolygon, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: PBLPoint; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APolygon, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @APolygon.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: TBLArrayView<TBLPointI>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @APolygon.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APolygon, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolygon(const APolygon: PBLPointI; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APolygon, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @APolyline.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: TBLArrayView<TBLPoint>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @APolyline.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APolyline, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: PBLPoint; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APolyline, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @APolyline.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: TBLArrayView<TBLPointI>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @APolyline.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APolyline, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddPolyline(const APolyline: PBLPointI; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APolyline, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddRect(const AX, AY, AW, AH: Double;
  const ADirection: TBLGeometryDirection);
var
  Rect: TBLRect;
begin
  Rect.Reset(AX, AY, AW, AH);
  _BLCheck(blPathAddRectD(@FHandle, @Rect, Ord(ADirection)));
end;

procedure TBLPath.AddRect(const ARect: TBLRect;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddRectD(@FHandle, @ARect, Ord(ADirection)));
end;

procedure TBLPath.AddRect(const ARect: TBLRectI;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddRectI(@FHandle, @ARect, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: PBLRectI; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: TBLArrayView<TBLRectI>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @ARects.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: TBLArrayView<TBLRectI>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @ARects.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: PBLRectI; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: PBLRect; const ACount: Integer;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: PBLRect; const ACount: Integer;
  const ADirection: TBLGeometryDirection);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: TBLArrayView<TBLRect>;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @ARects.FHandle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddRectArray(const ARects: TBLArrayView<TBLRect>;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @ARects.FHandle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRegion(const ARegion: IBLRegion;
  const ADirection: TBLGeometryDirection);
begin
  if (ARegion <> nil) then
    _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_REGION, ARegion.Handle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRegion(const ARegion: IBLRegion;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  if (ARegion <> nil) then
    _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_REGION, ARegion.Handle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddReversedPath(const APath: IBLPath;
  const AReverseMode: TBLPathReverseMode);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddReversedPath(@FHandle, APath.Handle, nil, Ord(AReverseMode)));
end;

procedure TBLPath.AddReversedPath(const APath: IBLPath; const ARange: TBLRange;
  const AReverseMode: TBLPathReverseMode);
begin
  if (APath <> nil) then
    _BLCheck(blPathAddReversedPath(@FHandle, APath.Handle, @ARange, Ord(AReverseMode)));
end;

procedure TBLPath.AddRoundRect(const ARoundRect: TBLRoundRect;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @ARoundRect, nil, Ord(ADirection)));
end;

procedure TBLPath.AddRoundRect(const ARoundRect: TBLRoundRect;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @ARoundRect, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.AddStrokedPath(const APath: IBLPath;
  const AStrokeOptions: TBLStrokeOptions;
  const AApproximationOptions: TBLApproximationOptions);
begin
  if (APath <> nil) then
  begin
    _BLCheck(blPathAddStrokedPath(@FHandle, APath.Handle, nil,
      @AStrokeOptions.FHandle, @AApproximationOptions.FHandle));
  end;
end;

procedure TBLPath.AddStrokedPath(const APath: IBLPath; const ARange: TBLRange;
  const AStrokeOptions: TBLStrokeOptions;
  const AApproximationOptions: TBLApproximationOptions);
begin
  if (APath <> nil) then
  begin
    _BLCheck(blPathAddStrokedPath(@FHandle, APath.Handle, @ARange,
      @AStrokeOptions.FHandle, @AApproximationOptions.FHandle));
  end;
end;

procedure TBLPath.AddTriangle(const ATriangle: TBLTriangle;
  const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @ATriangle, nil, Ord(ADirection)));
end;

procedure TBLPath.AddTriangle(const ATriangle: TBLTriangle;
  const AMatrix: TBLMatrix2D; const ADirection: TBLGeometryDirection);
begin
  _BLCheck(blPathAddGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @ATriangle, @AMatrix, Ord(ADirection)));
end;

procedure TBLPath.ArcQuadrantTo(const AX1, AY1, AX2, AY2: Double);
begin
  _BLCheck(blPathArcQuadrantTo(@FHandle, AX1, AY1, AX2, AY2));
end;

procedure TBLPath.ArcQuadrantTo(const AP1, AP2: TBLPoint);
begin
  _BLCheck(blPathArcQuadrantTo(@FHandle, AP1.FHandle.x, AP1.FHandle.y, AP2.FHandle.x, AP2.FHandle.y));
end;

procedure TBLPath.ArcTo(const AC, AR: TBLPoint; const AStart, ASweep: Double;
  const AForceMoveTo: Boolean);
begin
  _BLCheck(blPathArcTo(@FHandle, AC.FHandle.x, AC.FHandle.y, AR.FHandle.x,
    AR.FHandle.y, AStart, ASweep, AForceMoveTo));
end;

procedure TBLPath.ArcTo(const ACX, ACY, ARX, ARY, AStart, ASweep: Double;
  const AForceMoveTo: Boolean);
begin
  _BLCheck(blPathArcTo(@FHandle, ACX, ACY, ARX, ARY, AStart, ASweep, AForceMoveTo));
end;

procedure TBLPath.Clear;
begin
  _BLCheck(blPathClear(@FHandle));
end;

function TBLPath.Clone: IBLPath;
begin
  Result := TBLPath.Create;
  _BLCheck(blPathAssignDeep(Result.Handle, @FHandle));
end;

procedure TBLPath.Close;
begin
  _BLCheck(blPathClose(@FHandle));
end;

constructor TBLPath.Create;
begin
  inherited Create;
  blPathInit(@FHandle);
end;

procedure TBLPath.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double);
begin
  _BLCheck(blPathCubicTo(@FHandle, AX1, AY1, AX2, AY2, AX3, AY3));
end;

procedure TBLPath.CubicTo(const AP1, AP2, AP3: TBLPoint);
begin
  _BLCheck(blPathCubicTo(@FHandle, AP1.FHandle.x, AP1.FHandle.y, AP2.FHandle.x,
    AP2.FHandle.y, AP3.FHandle.x, AP3.FHandle.y));
end;

destructor TBLPath.Destroy;
begin
  blPathDestroy(@FHandle);
  inherited;
end;

procedure TBLPath.EllipticArcTo(const ARX, ARY, AXAxisRotation: Double;
  const ALargeArcFlag, ASweepFlag: Boolean; const AX1, AY1: Double);
begin
  _BLCheck(blPathEllipticArcTo(@FHandle, ARX, ARY, AXAxisRotation, ALargeArcFlag,
    ASweepFlag, AX1, AY1));
end;

procedure TBLPath.EllipticArcTo(const ARP: TBLPoint;
  const AXAxisRotation: Double; const ALargeArcFlag, ASweepFlag: Boolean;
  const AP1: TBLPoint);
begin
  _BLCheck(blPathEllipticArcTo(@FHandle, ARP.FHandle.x, ARP.FHandle.y,
    AXAxisRotation, ALargeArcFlag, ASweepFlag, AP1.FHandle.x, AP1.FHandle.y));
end;

function TBLPath.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLPath) then
    Result := blPathEquals(@FHandle, @TBLPath(Obj).FHandle)
  else
    Result := False;
end;

function TBLPath.Equals(const AOther: IBLPath): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blPathEquals(@FHandle, AOther.Handle);
end;

procedure TBLPath.FitTo(const ARect: TBLRect; const AFitFlags: TBLFitFlags);
begin
  _BLCheck(blPathFitTo(@FHandle, nil, @ARect, Byte(AFitFlags)));
end;

procedure TBLPath.FitTo(const ARange: TBLRange; const ARect: TBLRect;
  const AFitFlags: TBLFitFlags);
begin
  _BLCheck(blPathFitTo(@FHandle, @ARange, @ARect, Byte(AFitFlags)));
end;

function TBLPath.GetBoundingBox: TBLBox;
begin
  _BLCheck(blPathGetBoundingBox(@FHandle, @Result));
end;

function TBLPath.GetCapacity: Integer;
begin
  Result := FHandle.impl.capacity;
end;

function TBLPath.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): Integer;
var
  Index: NativeUInt;
  ActualDistance: Double;
begin
  _BLCheck(blPathGetClosestVertex(@FHandle, @AP, AMaxDistance, @Index, @ActualDistance));
  Result := Index;
end;

function TBLPath.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double; out AActualDistance: Double): Integer;
var
  Index: NativeUInt;
begin
  _BLCheck(blPathGetClosestVertex(@FHandle, @AP, AMaxDistance, @Index, @AActualDistance));
  Result := Index;
end;

function TBLPath.GetCommandData: PBLPathCmd;
begin
  Result := Pointer(FHandle.impl.commandData);
end;

function TBLPath.GetCommandDataEnd: PBLPathCmd;
begin
  Result := Pointer(FHandle.impl.commandData + FHandle.impl.size);
end;

function TBLPath.GetControlBox: TBLBox;
begin
  _BLCheck(blPathGetControlBox(@FHandle, @Result));
end;

function TBLPath.GetCount: Integer;
begin
  Result := FHandle.impl.size;
end;

function TBLPath.GetFigureRange(const AIndex: Integer): TBLRange;
begin
  _BLCheck(blPathGetFigureRange(@FHandle, AIndex, @Result));
end;

function TBLPath.GetHandle: PBLPathCore;
begin
  Result := @FHandle;
end;

function TBLPath.GetInfoFlags: TBLPathFlags;
begin
  _BLCheck(blPathGetInfoFlags(@FHandle, @Result));
end;

function TBLPath.GetIsEmpty: Boolean;
begin
  Result := (FHandle.impl.size = 0);
end;

function TBLPath.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLPath.GetLastVertex: TBLPoint;
begin
  _BLCheck(blPathGetLastVertex(@FHandle, @Result));
end;

function TBLPath.GetVertexData: PBLPoint;
begin
  Result := Pointer(FHandle.impl.vertexData);
end;

function TBLPath.GetVertexDataEnd: PBLPoint;
begin
  Result := Pointer(FHandle.impl.vertexData + FHandle.impl.size);
end;

function TBLPath.GetView: TBLPathView;
begin
  Result.FHandle := FHandle.impl.view;
end;

function TBLPath.HitTest(const AP: TBLPoint;
  const AFillRule: TBLFillRule): TBLHitTest;
begin
  Result := TBLHitTest(blPathHitTest(@FHandle, @AP, Ord(AFillRule)));
end;

procedure TBLPath.LineTo(const AX1, AY1: Double);
begin
  _BLCheck(blPathLineTo(@FHandle, AX1, AY1));
end;

procedure TBLPath.LineTo(const AP1: TBLPoint);
begin
  _BLCheck(blPathLineTo(@FHandle, AP1.FHandle.x, AP1.FHandle.y));
end;

procedure TBLPath.MoveTo(const AX0, AY0: Double);
begin
  _BLCheck(blPathMoveTo(@FHandle, AX0, AY0));
end;

procedure TBLPath.MoveTo(const AP0: TBLPoint);
begin
  _BLCheck(blPathMoveTo(@FHandle, AP0.FHandle.x, AP0.FHandle.y));
end;

procedure TBLPath.PolyTo(const APoly: TArray<TBLPoint>);
begin
  _BLCheck(blPathPolyTo(@FHandle, Pointer(APoly), Length(APoly)));
end;

procedure TBLPath.PolyTo(const APoly: PBLPoint; const ACount: Integer);
begin
  _BLCheck(blPathPolyTo(@FHandle, _PBLPoint(APoly), ACount));
end;

procedure TBLPath.QuadTo(const AP1, AP2: TBLPoint);
begin
  _BLCheck(blPathQuadTo(@FHandle, AP1.FHandle.x, AP1.FHandle.y, AP2.FHandle.x, AP2.FHandle.y));
end;

procedure TBLPath.QuadTo(const AX1, AY1, AX2, AY2: Double);
begin
  _BLCheck(blPathQuadTo(@FHandle, AX1, AY1, AX2, AY2));
end;

procedure TBLPath.RemoveRange(const ARange: TBLRange);
begin
  _BLCheck(blPathRemoveRange(@FHandle, @ARange.FHandle));
end;

procedure TBLPath.Reserve(const ACount: Integer);
begin
  _BLCheck(blPathReserve(@FHandle, ACount));
end;

procedure TBLPath.Reset;
begin
  _BLCheck(blPathReset(@FHandle));
end;

procedure TBLPath.SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
  const AX, AY: Double; const APreserve: Boolean);
const
  FLAGS: array [Boolean] of Cardinal = (0, BL_PATH_CMD_PRESERVE);
begin
  _BLCheck(blPathSetVertexAt(@FHandle, AIndex, Byte(ACmd) or FLAGS[APreserve], AX, AY));
end;

procedure TBLPath.SetVertexAt(const AIndex: Integer; const ACmd: TBLPathCmd;
  const APt: TBLPoint; const APreserve: Boolean);
const
  FLAGS: array [Boolean] of Cardinal = (0, BL_PATH_CMD_PRESERVE);
begin
  _BLCheck(blPathSetVertexAt(@FHandle, AIndex, Byte(ACmd) or FLAGS[APreserve],
    APt.FHandle.x, APt.FHandle.y));
end;

procedure TBLPath.Shrink;
begin
  _BLCheck(blPathShrink(@FHandle));
end;

procedure TBLPath.SmoothCubicTo(const AX2, AY2, AX3, AY3: Double);
begin
  _BLCheck(blPathSmoothCubicTo(@FHandle, AX2, AY2, AX3, AY3));
end;

procedure TBLPath.SmoothCubicTo(const AP2, AP3: TBLPoint);
begin
  _BLCheck(blPathSmoothCubicTo(@FHandle, AP2.FHandle.x, AP2.FHandle.y, AP3.FHandle.x, AP3.FHandle.y));
end;

procedure TBLPath.SmoothQuadTo(const AP2: TBLPoint);
begin
  _BLCheck(blPathSmoothQuadTo(@FHandle, AP2.FHandle.x, AP2.FHandle.y));
end;

procedure TBLPath.SmoothQuadTo(const AX2, AY2: Double);
begin
  _BLCheck(blPathSmoothQuadTo(@FHandle, AX2, AY2));
end;

procedure TBLPath.Transform(const ARange: TBLRange; const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blPathTransform(@FHandle, @ARange, @AMatrix));
end;

procedure TBLPath.Transform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blPathTransform(@FHandle, nil, @AMatrix));
end;

procedure TBLPath.Translate(const ARange: TBLRange; const AP: TBLPoint);
begin
  _BLCheck(blPathTranslate(@FHandle, @ARange, @AP));
end;

procedure TBLPath.Translate(const AP: TBLPoint);
begin
  _BLCheck(blPathTranslate(@FHandle, nil, @AP));
end;

{$ENDREGION 'Path'}

{$REGION 'Format'}

{ TBLFormatInfo }

class operator TBLFormatInfo.Equal(const ALeft, ARight: TBLFormatInfo): Boolean;
begin
  Result := CompareMem(@ALeft, @ARight, SizeOf(TBLFormatInfo));
end;

function TBLFormatInfo.GetFlags: TBLFormatFlags;
begin
  Cardinal(Result) := FHandle.flags;
end;

function TBLFormatInfo.GetPalette: PBLRgba32;
begin
  Result := Pointer(FHandle.palette);
end;

class operator TBLFormatInfo.NotEqual(const ALeft,
  ARight: TBLFormatInfo): Boolean;
begin
  Result := not CompareMem(@ALeft, @ARight, SizeOf(TBLFormatInfo));
end;

procedure TBLFormatInfo.Query(const AFormat: TBLFormat);
begin
  _BLCheck(blFormatInfoQuery(@FHandle, Ord(AFormat)));
end;

procedure TBLFormatInfo.Reset(const ADepth: Integer;
  const AFlags: TBLFormatFlags; const ARSize, AGSize, ABSize, AASize, ARShift,
  AGShift, ABShift, AAShift: Byte);
begin
  FHandle.depth := ADepth;
  FHandle.flags := Cardinal(AFlags);
  FHandle.rSize := ARSize;
  FHandle.gSize := AGSize;
  FHandle.bSize := ABSize;
  FHandle.aSize := AASize;
  FHandle.rShift := ARShift;
  FHandle.gShift := AGShift;
  FHandle.bShift := ABShift;
  FHandle.aShift := AAShift;
end;

procedure TBLFormatInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFormatInfo.Sanitize;
begin
  _BLCheck(blFormatInfoSanitize(@FHandle));
end;

procedure TBLFormatInfo.SetFlags(const AValue: TBLFormatFlags);
begin
  FHandle.flags := Cardinal(AValue);
end;

procedure TBLFormatInfo.SetShifts(const ARShift, AGShift, ABShift,
  AAShift: Byte);
begin
  FHandle.rShift := ARShift;
  FHandle.gShift := AGShift;
  FHandle.bShift := ABShift;
  FHandle.aShift := AAShift;
end;

procedure TBLFormatInfo.SetSizes(const ARSize, AGSize, ABSize, AASize: Byte);
begin
  FHandle.rSize := ARSize;
  FHandle.gSize := AGSize;
  FHandle.bSize := ABSize;
  FHandle.aSize := AASize;
end;

{ _TBLFormatHelper }

function _TBLFormatHelper.GetInfo: TBLFormatInfo;
begin
  _BLCheck(blFormatInfoQuery(@Result.FHandle, Ord(Self)));
end;

{$ENDREGION 'Format'}

{$REGION 'Image'}

{ TBLImageData }

function TBLImageData.GetFlags: TBLFormatFlags;
begin
  Cardinal(Result) := FHandle.flags;
end;

function TBLImageData.GetFormat: TBLFormat;
begin
  Byte(Result) := FHandle.format;
end;

function TBLImageData.GetSize: TBLSizeI;
begin
  Result.FHandle := FHandle.size;
end;

procedure TBLImageData.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLImageData.SetFlags(const AValue: TBLFormatFlags);
begin
  FHandle.flags := Cardinal(AValue);
end;

procedure TBLImageData.SetFormat(const AValue: TBLFormat);
begin
  FHandle.format := Ord(AValue);
end;

procedure TBLImageData.SetSize(const AValue: TBLSizeI);
begin
  FHandle.size := AValue.FHandle;
end;

{ TBLImageInfo }

function TBLImageInfo.GetCompression: String;
begin
  Result := String(UTF8String(FHandle.compression));
end;

function TBLImageInfo.GetDensity: TBLSize;
begin
  Result.FHandle := FHandle.density;
end;

function TBLImageInfo.GetDepth: Integer;
begin
  Result := FHandle.depth;
end;

function TBLImageInfo.GetFlags: TBLFormatFlags;
begin
  Cardinal(Result) := FHandle.flags;
end;

function TBLImageInfo.GetFormat: String;
begin
  Result := String(UTF8String(FHandle.format));
end;

function TBLImageInfo.GetFrameCount: Integer;
begin
  Result := FHandle.frameCount;
end;

function TBLImageInfo.GetPlaneCount: Integer;
begin
  Result := FHandle.planeCount;
end;

function TBLImageInfo.GetSize: TBLSizeI;
begin
  Result.FHandle := FHandle.size;
end;

procedure TBLImageInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLImageScaleOptions }

function TBLImageScaleOptions.GetUserFunc: TBLImageScaleUserFunc;
begin
  Result := TBLImageScaleUserFunc(FHandle.userFunc);
end;

procedure TBLImageScaleOptions.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLImageScaleOptions.ResetToDefaults;
begin
  FHandle.userFunc := nil;
  FHandle.userData := nil;
  FHandle.radius := 2;
  FHandle.mitchell.b := 1 / 3;
  FHandle.mitchell.c := 1 / 3;
  FHandle.data[2] := 0;
end;

procedure TBLImageScaleOptions.SetUserFunc(const AValue: TBLImageScaleUserFunc);
begin
  TBLImageScaleUserFunc(FHandle.userFunc) := AValue;
end;

{ TBLImage }

constructor TBLImage.Create;
begin
  inherited Create;
  blImageInit(@FHandle);
end;

function TBLImage.Clone: IBLImage;
begin
  Result := TBLImage.Create;
  _BLCheck(blImageAssignDeep(Result.Handle, @FHandle));
end;

procedure TBLImage.Convert(const AFormat: TBLFormat);
begin
  _BLCheck(blImageConvert(@FHandle, Ord(AFormat)));
end;

constructor TBLImage.Create(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat);
begin
  inherited Create;
  _BLCheck(blImageInitAs(@FHandle, AWidth, AHeight, Ord(AFormat)));
end;

constructor TBLImage.Create(const AHandle: BLImageCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
end;

destructor TBLImage.Destroy;
begin
  if (not FIsReference) then
    blImageDestroy(@FHandle);
  inherited;
end;

class procedure TBLImage.DoDestroy(impl, destroyData: Pointer);
var
  Data: PDestroyData absolute destroyData;
begin
  if (Data <> nil) then
  begin
    Data.Event(Data.Image);
    FreeMem(Data);
  end;
end;

function TBLImage.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLImage) then
    Result := blImageEquals(@FHandle, @TBLImage(Obj).FHandle)
  else
    Result := False;
end;

function TBLImage.Equals(const AOther: IBLImage): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blImageEquals(@FHandle, AOther.Handle);
end;

procedure TBLImage.GetData(out AData: TBLImageData);
begin
  _BLCheck(blImageGetData(@FHandle, @AData.FHandle));
end;

function TBLImage.GetFormat: TBLFormat;
begin
  Result := TBLFormat(FHandle.impl.format);
end;

function TBLImage.GetHandle: PBLImageCore;
begin
  Result := @FHandle;
end;

function TBLImage.GetHeight: Integer;
begin
  Result := FHandle.impl.size.h;
end;

function TBLImage.GetIsEmpty: Boolean;
begin
  Result := (FHandle.impl.format = BL_FORMAT_NONE);
end;

function TBLImage.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLImage.GetSize: TBLSizeI;
begin
  Result := TBLSizeI(FHandle.impl.size);
end;

function TBLImage.GetWidth: Integer;
begin
  Result := FHandle.impl.size.w;
end;

procedure TBLImage.Initialize(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat);
begin
  _BLCheck(blImageCreate(@FHandle, AWidth, AHeight, Ord(AFormat)));
end;

procedure TBLImage.InitializeFromData(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat; const APixelData: Pointer; const AStride: Integer;
  const AOnDestroy: TBLImageDestroyEvent);
var
  DestroyFunc: BLDestroyImplFunc;
  DestroyData: PDestroyData;
begin
  if Assigned(AOnDestroy) then
  begin
    DestroyFunc := DoDestroy;
    GetMem(DestroyData, SizeOf(TDestroyData));
    DestroyData.Image := Self;
    DestroyData.Event := AOnDestroy;
  end
  else
  begin
    DestroyFunc := nil;
    DestroyData := nil;
  end;
  _BLCheck(blImageCreateFromData(@FHandle, AWidth, AHeight, Ord(AFormat),
    APixelData, AStride, DestroyFunc, DestroyData));
end;

procedure TBLImage.MakeMutable(out AData: TBLImageData);
begin
  _BLCheck(blImageMakeMutable(@FHandle, @AData.FHandle));
end;

procedure TBLImage.MakeMutable;
var
  Unused: TBLImageData;
begin
  _BLCheck(blImageMakeMutable(@FHandle, @Unused.FHandle));
end;

procedure TBLImage.ReadFromData(const AData: TBytes);
begin
  ReadFromData(Pointer(AData), Length(AData), nil);
end;

procedure TBLImage.ReadFromData(const AData: TBytes;
  const ACodecs: TArray<IBLImageCodec>);
begin
  ReadFromData(Pointer(AData), Length(AData), ACodecs);
end;

procedure TBLImage.ReadFromData(const AData: Pointer; const ASize: Integer);
begin
  _BLCheck(blImageReadFromData(@FHandle, AData, ASize, nil));
end;

procedure TBLImage.ReadFromData(const AData: Pointer; const ASize: Integer;
  const ACodecs: TArray<IBLImageCodec>);
var
  CodecHandles: TArray<BLImageCodecCore>;
  Codecs: IBLArray;
  I: Integer;
begin
  SetLength(CodecHandles, Length(ACodecs));
  for I := 0 to Length(CodecHandles) - 1 do
    CodecHandles[I] := ACodecs[I].Handle^;
  Codecs := TBLUtils.ArrayToBLArray<BLImageCodecCore>(CodecHandles);

  _BLCheck(blImageReadFromData(@FHandle, AData, ASize, Codecs.Handle));
end;

procedure TBLImage.ReadFromData(const AView: TBLArrayView<Byte>);
begin
  ReadFromData(AView.Data, AView.Length, nil);
end;

procedure TBLImage.ReadFromData(const AView: TBLArrayView<Byte>;
  const ACodecs: TArray<IBLImageCodec>);
begin
  ReadFromData(AView.Data, AView.Length, ACodecs);
end;

procedure TBLImage.ReadFromFile(const AFilename: String);
begin
  _BLCheck(blImageReadFromFile(@FHandle, MarshaledAString(UTF8String(AFilename)), nil));
end;

procedure TBLImage.ReadFromFile(const AFilename: String;
  const ACodecs: TArray<IBLImageCodec>);
var
  CodecHandles: TArray<BLImageCodecCore>;
  Codecs: IBLArray;
  I: Integer;
begin
  SetLength(CodecHandles, Length(ACodecs));
  for I := 0 to Length(CodecHandles) - 1 do
    CodecHandles[I] := ACodecs[I].Handle^;
  Codecs := TBLUtils.ArrayToBLArray<BLImageCodecCore>(CodecHandles);

  _BLCheck(blImageReadFromFile(@FHandle, MarshaledAString(UTF8String(AFilename)), Codecs.Handle));
end;

procedure TBLImage.Reset;
begin
  _BLCheck(blImageReset(@FHandle));
end;

class procedure TBLImage.Scale(const ASrc, ADst: IBLImage;
  const ASize: TBLSizeI; const AFilter: TBLImageScaleFilter);
begin
  if (ASrc <> nil) then
    ASrc.ScaleTo(ADst, ASize, AFilter);
end;

class procedure TBLImage.Scale(const ASrc, ADst: IBLImage;
  const ASize: TBLSizeI; const AFilter: TBLImageScaleFilter;
  const AOptions: TBLImageScaleOptions);
begin
  if (ASrc <> nil) then
    ASrc.ScaleTo(ADst, ASize, AFilter, AOptions);
end;

procedure TBLImage.ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
  const AFilter: TBLImageScaleFilter);
begin
  if (ADest <> nil) then
    _BLCheck(blImageScale(ADest.Handle, @FHandle, @ASize, Ord(AFilter), nil));
end;

procedure TBLImage.ScaleTo(const ADest: IBLImage; const ASize: TBLSizeI;
  const AFilter: TBLImageScaleFilter; const AOptions: TBLImageScaleOptions);
begin
  if (ADest <> nil) then
    _BLCheck(blImageScale(ADest.Handle, @FHandle, @ASize, Ord(AFilter), @AOptions.FHandle));
end;

function TBLImage.WriteToData(const ACodec: IBLImageCodec): TBytes;
var
  Codec: PBLImageCodecCore;
  Data: IBLArray;
begin
  if (ACodec = nil) then
    Codec := nil
  else
    Codec := ACodec.Handle;

  Data := TBLUtils.CreateBLArray<Byte>;
  _BLCheck(blImageWriteToData(@FHandle, Data.Handle, Codec));
  Result := TBLUtils.BLArrayToArray<Byte>(Data);
end;

procedure TBLImage.WriteToFile(const AFilename: String);
begin
  _BLCheck(blImageWriteToFile(@FHandle, MarshaledAString(UTF8String(AFilename)), nil));
end;

procedure TBLImage.WriteToFile(const AFilename: String;
  const ACodec: IBLImageCodec);
var
  Codec: PBLImageCodecCore;
begin
  if (ACodec = nil) then
    Codec := nil
  else
    Codec := ACodec.Handle;

  _BLCheck(blImageWriteToFile(@FHandle, MarshaledAString(UTF8String(AFilename)), Codec));
end;

{$ENDREGION 'Image'}

{$REGION 'Image Codec'}

{ TBLImageDecoder }

constructor TBLImageDecoder.Create;
begin
  inherited Create;
  blImageDecoderInit(@FHandle);
end;

destructor TBLImageDecoder.Destroy;
begin
  blImageDecoderDestroy(@FHandle);
  inherited;
end;

function TBLImageDecoder.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLImageDecoder) then
    Result := (FHandle.impl = @TBLImageDecoder(Obj).FHandle.impl)
  else
    Result := False;
end;

function TBLImageDecoder.Equals(const AOther: IBLImageDecoder): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := (FHandle.impl = AOther.Handle.impl);
end;

function TBLImageDecoder.GetBufferIndex: NativeInt;
begin
  Result := FHandle.impl.bufferIndex;
end;

function TBLImageDecoder.GetFrameIndex: Integer;
begin
  Result := FHandle.impl.frameIndex;
end;

function TBLImageDecoder.GetHandle: PBLImageDecoderCore;
begin
  Result := @FHandle;
end;

function TBLImageDecoder.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLImageDecoder.GetLastResult: TBLResultCode;
begin
  Result := TBLResultCode(FHandle.impl.lastResult);
end;

function TBLImageDecoder.ReadFrame(const ABuffer: Pointer;
  const ASize: Integer): IBLImage;
begin
  Result := TBLImage.Create;
  _BLCheck(blImageDecoderReadFrame(@FHandle, Result.Handle, ABuffer, ASize));
end;

function TBLImageDecoder.ReadFrame(const ABuffer: TBLArrayView<Byte>): IBLImage;
begin
  Result := TBLImage.Create;
  _BLCheck(blImageDecoderReadFrame(@FHandle, Result.Handle,
    ABuffer.FHandle.data, ABuffer.FHandle.size));
end;

function TBLImageDecoder.ReadFrame(const ABuffer: TBytes): IBLImage;
begin
  Result := TBLImage.Create;
  _BLCheck(blImageDecoderReadFrame(@FHandle, Result.Handle,
    Pointer(ABuffer), Length(ABuffer)));
end;

procedure TBLImageDecoder.ReadInfo(const ABuffer: Pointer; const ASize: Integer;
  out AInfo: TBLImageInfo);
begin
  _BLCheck(blImageDecoderReadInfo(@FHandle, @AInfo.FHandle, ABuffer, ASize));
end;

procedure TBLImageDecoder.ReadInfo(const ABuffer: TBLArrayView<Byte>;
  out AInfo: TBLImageInfo);
begin
  _BLCheck(blImageDecoderReadInfo(@FHandle, @AInfo.FHandle,
    ABuffer.FHandle.data, ABuffer.FHandle.size));
end;

procedure TBLImageDecoder.ReadInfo(const ABuffer: TBytes;
  out AInfo: TBLImageInfo);
begin
  _BLCheck(blImageDecoderReadInfo(@FHandle, @AInfo.FHandle,
    Pointer(ABuffer), Length(ABuffer)));
end;

procedure TBLImageDecoder.Reset;
begin
  _BLCheck(blImageDecoderReset(@FHandle));
end;

procedure TBLImageDecoder.Restart;
begin
  _BLCheck(blImageDecoderRestart(@FHandle));
end;

{ TBLImageEncoder }

constructor TBLImageEncoder.Create;
begin
  inherited Create;
  blImageEncoderInit(@FHandle);
end;

destructor TBLImageEncoder.Destroy;
begin
  blImageEncoderDestroy(@FHandle);
  inherited;
end;

function TBLImageEncoder.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLImageEncoder) then
    Result := (FHandle.impl = @TBLImageEncoder(Obj).FHandle.impl)
  else
    Result := False;
end;

function TBLImageEncoder.Equals(const AOther: IBLImageEncoder): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := (FHandle.impl = AOther.Handle.impl);
end;

function TBLImageEncoder.GetBufferIndex: NativeInt;
begin
  Result := FHandle.impl.bufferIndex;
end;

function TBLImageEncoder.GetFrameIndex: Integer;
begin
  Result := FHandle.impl.frameIndex;
end;

function TBLImageEncoder.GetHandle: PBLImageEncoderCore;
begin
  Result := @FHandle;
end;

function TBLImageEncoder.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLImageEncoder.GetLastResult: TBLResultCode;
begin
  Result := TBLResultCode(FHandle.impl.lastResult);
end;

procedure TBLImageEncoder.Reset;
begin
  _BLCheck(blImageEncoderReset(@FHandle));
end;

procedure TBLImageEncoder.Restart;
begin
  _BLCheck(blImageEncoderRestart(@FHandle));
end;

function TBLImageEncoder.WriteFrame(const AImage: IBLImage): TBytes;
var
  Dest: IBLArray;
begin
  if (AImage <> nil) then
  begin
    Dest := TBLUtils.CreateBLArray<Byte>;
    _BLCheck(blImageEncoderWriteFrame(@FHandle, Dest.Handle, AImage.Handle));
    Result := TBLUtils.BLArrayToArray<Byte>(Dest);
  end;
end;

{ TBLImageCodec }

class procedure TBLImageCodec.AddToBuiltIn(const ACodec: IBLImageCodec);
begin
  if (ACodec <> nil) then
    _BLCheck(blImageCodecAddToBuiltIn(ACodec.Handle));
end;

constructor TBLImageCodec.Create;
begin
  inherited Create;
  blImageCodecInit(@FHandle);
end;

constructor TBLImageCodec.Create(const AHandle: BLImageCodecCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
end;

destructor TBLImageCodec.Destroy;
begin
  if (not FIsReference) then
    blImageCodecDestroy(@FHandle);
  inherited;
end;

function TBLImageCodec.Equals(const AOther: IBLImageCodec): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := (FHandle.impl = AOther.Handle.impl);
end;

function TBLImageCodec.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLImageCodec) then
    Result := (FHandle.impl = @TBLImageCodec(Obj).FHandle.impl)
  else
    Result := False;
end;

function TBLImageCodec.FindByData(const AData: TBytes;
  const ACodecs: TArray<IBLImageCodec>): Boolean;
begin
  Result := FindByData(Pointer(AData), Length(AData), ACodecs);
end;

function TBLImageCodec.FindByExtension(const AExt: String;
  const ACodecs: TArray<IBLImageCodec>): Boolean;
var
  CodecHandles: TArray<BLImageCodecCore>;
  CodecArray: IBLArray;
  Codecs: PBLArrayCore;
  Ext: UTF8String;
  Res: BLResultCode;
  I: Integer;
begin
  if Assigned(ACodecs) then
  begin
    SetLength(CodecHandles, Length(ACodecs));
    for I := 0 to Length(CodecHandles) - 1 do
      CodecHandles[I] := ACodecs[I].Handle^;
    CodecArray := TBLUtils.ArrayToBLArray<BLImageCodecCore>(CodecHandles);
    Codecs := CodecArray.Handle;
  end
  else
    Codecs := nil;

  Ext := UTF8String(AExt);
  Res := blImageCodecFindByExtension(@FHandle, MarshaledAString(Ext), Length(Ext), Codecs);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByExtension(const AExt: String): Boolean;
var
  Ext: UTF8String;
  Res: BLResultCode;
begin
  Ext := UTF8String(AExt);
  Res := blImageCodecFindByExtension(@FHandle, MarshaledAString(Ext), Length(Ext), nil);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByData(const AData: Pointer;
  const ASize: Integer): Boolean;
var
  Res: BLResultCode;
begin
  Res := blImageCodecFindByData(@FHandle, AData, ASize, nil);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByData(const AData: Pointer; const ASize: Integer;
  const ACodecs: TArray<IBLImageCodec>): Boolean;
var
  CodecHandles: TArray<BLImageCodecCore>;
  CodecArray: IBLArray;
  Codecs: PBLArrayCore;
  Res: BLResultCode;
  I: Integer;
begin
  if Assigned(ACodecs) then
  begin
    SetLength(CodecHandles, Length(ACodecs));
    for I := 0 to Length(CodecHandles) - 1 do
      CodecHandles[I] := ACodecs[I].Handle^;
    CodecArray := TBLUtils.ArrayToBLArray<BLImageCodecCore>(CodecHandles);
    Codecs := CodecArray.Handle;
  end
  else
    Codecs := nil;

  Res := blImageCodecFindByData(@FHandle, AData, ASize, Codecs);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByData(const AData: TBytes): Boolean;
var
  Res: BLResultCode;
begin
  Res := blImageCodecFindByData(@FHandle, Pointer(AData), Length(AData), nil);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByName(const AName: String;
  const ACodecs: TArray<IBLImageCodec>): Boolean;
var
  CodecHandles: TArray<BLImageCodecCore>;
  CodecArray: IBLArray;
  Codecs: PBLArrayCore;
  Name: UTF8String;
  Res: BLResultCode;
  I: Integer;
begin
  if Assigned(ACodecs) then
  begin
    SetLength(CodecHandles, Length(ACodecs));
    for I := 0 to Length(CodecHandles) - 1 do
      CodecHandles[I] := ACodecs[I].Handle^;
    CodecArray := TBLUtils.ArrayToBLArray<BLImageCodecCore>(CodecHandles);
    Codecs := CodecArray.Handle;
  end
  else
    Codecs := nil;

  Name := UTF8String(AName);
  Res := blImageCodecFindByName(@FHandle, MarshaledAString(Name), Length(Name), Codecs);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

function TBLImageCodec.FindByName(const AName: String): Boolean;
var
  Name: UTF8String;
  Res: BLResultCode;
begin
  Name := UTF8String(AName);
  Res := blImageCodecFindByName(@FHandle, MarshaledAString(Name), Length(Name), nil);
  Result := (Res = BL_SUCCESS);
  if (Res <> BL_ERROR_IMAGE_NO_MATCHING_CODEC) then
    _BLCheck(Res);
end;

class function TBLImageCodec.GetBuiltInCodecs: TArray<IBLImageCodec>;
var
  CodecArray: IBLArray;
  CodecHandles: TArray<BLImageCodecCore>;
  I: Integer;
begin
  CodecArray := TBLUtils.CreateBLArray<BLImageCodecCore>;
  _BLCheck(blImageCodecArrayInitBuiltInCodecs(CodecArray.Handle));
  CodecHandles := TBLUtils.BLArrayToArray<BLImageCodecCore>(CodecArray);

  SetLength(Result, Length(CodecHandles));
  for I := 0 to Length(Result) - 1 do
    Result[I] := TBLImageCodec.Create(CodecHandles[I], True);
end;

function TBLImageCodec.GetExtensions: String;
begin
  Result := String(UTF8String(FHandle.impl.extensions));
end;

function TBLImageCodec.GetFeatures: TBLImageCodecFeatures;
begin
  Result := TBLImageCodecFeatures(FHandle.impl.features);
end;

function TBLImageCodec.GetHandle: PBLImageCodecCore;
begin
  Result := @FHandle;
end;

function TBLImageCodec.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLImageCodec.GetMimeType: String;
begin
  Result := String(UTF8String(FHandle.impl.mimeType));
end;

function TBLImageCodec.GetName: String;
begin
  Result := String(UTF8String(FHandle.impl.name));
end;

function TBLImageCodec.GetVendor: String;
begin
  Result := String(UTF8String(FHandle.impl.vendor));
end;

function TBLImageCodec.HasFeature(
  const AFeature: TBLImageCodecFeature): Boolean;
begin
  Result := ((FHandle.impl.features and Ord(AFeature)) <> 0);
end;

function TBLImageCodec.InspectData(const ABuffer: TBytes): Cardinal;
begin
  Result := blImageCodecInspectData(@FHandle, Pointer(ABuffer), Length(ABuffer));
end;

function TBLImageCodec.InspectData(const ABuffer: TBLArrayView<Byte>): Cardinal;
begin
  Result := blImageCodecInspectData(@FHandle, ABuffer.FHandle.data, ABuffer.FHandle.size);
end;

function TBLImageCodec.InspectData(const ABuffer: Pointer;
  const ASize: Integer): Cardinal;
begin
  Result := blImageCodecInspectData(@FHandle, ABuffer, ASize);
end;

class procedure TBLImageCodec.RemoveFromBuiltIn(const ACodec: IBLImageCodec);
begin
  if (ACodec <> nil) then
    _BLCheck(blImageCodecRemoveFromBuiltIn(ACodec.Handle));
end;

procedure TBLImageCodec.Reset;
begin
  _BLCheck(blImageCodecReset(@FHandle));
end;

{$ENDREGION 'Image Codec'}

{$REGION 'Pattern'}

{ TBLPattern }

constructor TBLPattern.Create;
begin
  inherited;
  blPatternInit(@FHandle);
end;

constructor TBLPattern.Create(const AImage: IBLImage;
  const AExtendMode: TBLExtendMode);
begin
  inherited Create;
  FImage := AImage;
  if (AImage = nil) then
    blPatternInit(@FHandle)
  else
    _BLCheck(blPatternInitAs(@FHandle, AImage.Handle, nil, Ord(AExtendMode), nil));
end;

constructor TBLPattern.Create(const AImage: IBLImage;
  const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D);
begin
  inherited Create;
  FImage := AImage;
  if (AImage = nil) then
    blPatternInit(@FHandle)
  else
    _BLCheck(blPatternInitAs(@FHandle, AImage.Handle, nil, Ord(AExtendMode), @AMatrix));
end;

constructor TBLPattern.Create(const AImage: IBLImage; const AArea: TBLRectI;
  const AExtendMode: TBLExtendMode);
begin
  inherited Create;
  FImage := AImage;
  if (AImage = nil) then
    blPatternInit(@FHandle)
  else
    _BLCheck(blPatternInitAs(@FHandle, AImage.Handle, @AArea, Ord(AExtendMode), nil));
end;

constructor TBLPattern.Create(const AImage: IBLImage; const AArea: TBLRectI;
  const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D);
begin
  inherited Create;
  FImage := AImage;
  if (AImage = nil) then
    blPatternInit(@FHandle)
  else
    _BLCheck(blPatternInitAs(@FHandle, AImage.Handle, @AArea, Ord(AExtendMode), @AMatrix));
end;

constructor TBLPattern.Create(const AHandle: BLPatternCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
  FImage := TBLImage.Create(FHandle.impl.image, True);
end;

destructor TBLPattern.Destroy;
begin
  if (not FIsReference) then
    blPatternDestroy(@FHandle);
  inherited;
end;

function TBLPattern.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLPattern) then
    Result := blPatternEquals(@FHandle, @TBLPattern(Obj).FHandle)
  else
    Result := False;
end;

function TBLPattern.Equals(const AOther: IBLPattern): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blPatternEquals(@FHandle, AOther.Handle)
end;

function TBLPattern.GetArea: TBLRectI;
begin
  Result.FHandle := FHandle.impl.area;
end;

function TBLPattern.GetExtendMode: TBLExtendMode;
begin
  Result := TBLExtendMode(FHandle.impl.extendMode);
end;

function TBLPattern.GetHandle: PBLPatternCore;
begin
  Result := @FHandle;
end;

function TBLPattern.GetHasMatrix: Boolean;
begin
  Result := (FHandle.impl.matrixType <> BL_MATRIX2D_TYPE_IDENTITY);
end;

function TBLPattern.GetImage: IBLImage;
begin
  Result := FImage;
end;

function TBLPattern.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLPattern.GetMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.impl.matrix;
end;

function TBLPattern.GetMatrixType: TBLMatrix2DType;
begin
  Result := TBLMatrix2DType(FHandle.impl.matrixType);
end;

procedure TBLPattern.Initialize(const AImage: IBLImage; const AArea: TBLRectI;
  const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D);
begin
  FImage := AImage;
  if (AImage <> nil) then
    _BLCheck(blPatternCreate(@FHandle, AImage.Handle, @AArea, Ord(AExtendMode), @AMatrix));
end;

procedure TBLPattern.Initialize(const AImage: IBLImage;
  const AExtendMode: TBLExtendMode);
begin
  FImage := AImage;
  if (AImage <> nil) then
    _BLCheck(blPatternCreate(@FHandle, AImage.Handle, nil, Ord(AExtendMode), nil));
end;

procedure TBLPattern.Initialize(const AImage: IBLImage;
  const AExtendMode: TBLExtendMode; const AMatrix: TBLMatrix2D);
begin
  FImage := AImage;
  if (AImage <> nil) then
    _BLCheck(blPatternCreate(@FHandle, AImage.Handle, nil, Ord(AExtendMode), @AMatrix));
end;

procedure TBLPattern.Initialize(const AImage: IBLImage; const AArea: TBLRectI;
  const AExtendMode: TBLExtendMode);
begin
  FImage := AImage;
  if (AImage <> nil) then
    _BLCheck(blPatternCreate(@FHandle, AImage.Handle, @AArea, Ord(AExtendMode), nil));
end;

procedure TBLPattern.PostRotate(const AAngle: Double);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE, @AAngle));
end;

procedure TBLPattern.PostRotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLPattern.PostRotate(const AAngle: Double; const AP: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLPattern.PostRotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLPattern.PostScale(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @AP));
end;

procedure TBLPattern.PostScale(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLPattern.PostScale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLPattern.PostScale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLPattern.PostSkew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @Data));
end;

procedure TBLPattern.PostSkew(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @AP));
end;

procedure TBLPattern.PostTransform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSFORM, @AMatrix));
end;

procedure TBLPattern.PostTranslate(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @AP));
end;

procedure TBLPattern.PostTranslate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLPattern.PostTranslate(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLPattern.Reset;
begin
  _BLCheck(blPatternReset(@FHandle));
end;

procedure TBLPattern.ResetArea;
var
  Area: TBLRectI;
begin
  Area.Reset(0, 0, 0, 0);
  _BLCheck(blPatternSetArea(@FHandle, @Area));
end;

procedure TBLPattern.ResetExtendMode;
begin
  _BLCheck(blPatternSetExtendMode(@FHandle, BL_EXTEND_MODE_REPEAT));
end;

procedure TBLPattern.ResetImage;
begin
  FImage := TBLImage.Create;
  _BLCheck(blPatternSetImage(@FHandle, FImage.Handle, nil));
end;

procedure TBLPattern.ResetMatrix;
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_RESET, nil));
end;

procedure TBLPattern.Rotate(const AAngle: Double; const AP: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLPattern.Rotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLPattern.Rotate(const AAngle: Double);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE, @AAngle));
end;

procedure TBLPattern.Rotate(const AAngle: Double; const AP: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AP.FHandle.x;
  Data[2] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLPattern.Scale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLPattern.Scale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLPattern.Scale(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLPattern.Scale(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @AP));
end;

procedure TBLPattern.SetArea(const AValue: TBLRectI);
begin
  _BLCheck(blPatternSetArea(@FHandle, @AValue));
end;

procedure TBLPattern.SetExtendMode(const AValue: TBLExtendMode);
begin
  _BLCheck(blPatternSetExtendMode(@FHandle, Ord(AValue)));
end;

procedure TBLPattern.SetImage(const AValue: IBLImage; const AArea: TBLRectI);
begin
  if (AValue <> nil) then
    _BLCheck(blPatternSetImage(@FHandle, AValue.Handle, @AArea));
end;

procedure TBLPattern.SetImage(const AValue: IBLImage);
begin
  if (AValue <> nil) then
    _BLCheck(blPatternSetImage(@FHandle, AValue.Handle, nil));
end;

procedure TBLPattern.SetMatrix(const AValue: TBLMatrix2D);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_ASSIGN, @AValue));
end;

procedure TBLPattern.Skew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @Data));
end;

procedure TBLPattern.Skew(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @AP));
end;

procedure TBLPattern.Transform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSFORM, @AMatrix));
end;

procedure TBLPattern.Translate(const AP: TBLPoint);
begin
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @AP));
end;

procedure TBLPattern.Translate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

procedure TBLPattern.Translate(const AP: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AP.FHandle.x;
  Data[1] := AP.FHandle.y;
  _BLCheck(blPatternApplyMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

{$ENDREGION 'Pattern'}

{$REGION 'Font Defs'}

{ TBLGlyphInfo }

procedure TBLGlyphInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLGlyphPlacement }

function TBLGlyphPlacement.GetAdvance: TBLPointI;
begin
  Result.FHandle := FHandle.advance;
end;

function TBLGlyphPlacement.GetPlacement: TBLPointI;
begin
  Result.FHandle := FHandle.placement;
end;

procedure TBLGlyphPlacement.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLGlyphPlacement.SetAdvance(const AValue: TBLPointI);
begin
  FHandle.advance := AValue.FHandle;
end;

procedure TBLGlyphPlacement.SetPlacement(const AValue: TBLPointI);
begin
  FHandle.placement := AValue.FHandle;
end;

{ TBLGlyphMappingState }

function TBLGlyphMappingState.GetGlyphCount: Integer;
begin
  Result := FHandle.glyphCount;
end;

function TBLGlyphMappingState.GetUndefinedCount: Integer;
begin
  Result := FHandle.undefinedCount;
end;

function TBLGlyphMappingState.GetUndefinedFirst: Integer;
begin
  Result := FHandle.undefinedFirst;
end;

procedure TBLGlyphMappingState.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLGlyphMappingState.SetGlyphCount(const AValue: Integer);
begin
  FHandle.glyphCount := AValue;
end;

procedure TBLGlyphMappingState.SetUndefinedCount(const AValue: Integer);
begin
  FHandle.undefinedCount := AValue;
end;

procedure TBLGlyphMappingState.SetUndefinedFirst(const AValue: Integer);
begin
  FHandle.undefinedFirst := AValue;
end;

{ TBLGlyphOutlineSinkInfo }

function TBLGlyphOutlineSinkInfo.GetContourCount: Integer;
begin
  Result := FHandle.contourCount;
end;

function TBLGlyphOutlineSinkInfo.GetGlyphIndex: Integer;
begin
  Result := FHandle.glyphIndex;
end;

procedure TBLGlyphOutlineSinkInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLGlyphOutlineSinkInfo.SetContourCount(const AValue: Integer);
begin
  FHandle.contourCount := AValue;
end;

procedure TBLGlyphOutlineSinkInfo.SetGlyphIndex(const AValue: Integer);
begin
  FHandle.glyphIndex := AValue;
end;

{ TBLGlyphRun }

function TBLGlyphRun.Entries<TPlacement>: TEnumerable<TPlacement>;
begin
  Result := TEnumerable<TPlacement>.Create(@FHandle);
end;

function TBLGlyphRun.GetFlags: TBLGlyphRunFlags;
begin
  Cardinal(Result) := FHandle.flags;
end;

function TBLGlyphRun.GetIsEmpty: Boolean;
begin
  Result := (FHandle.size = 0);
end;

function TBLGlyphRun.GetPlacementType: TBLGlyphPlacementType;
begin
  Result := TBLGlyphPlacementType(FHandle.placementType);
end;

function TBLGlyphRun.GetSize: Integer;
begin
  Result := FHandle.size;
end;

procedure TBLGlyphRun.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLGlyphRun.ResetGlyphIdData;
begin
  FHandle.glyphData := nil;
  FHandle.glyphAdvance := 0;
end;

procedure TBLGlyphRun.ResetPlacementData;
begin
  FHandle.placementData := nil;
  FHandle.placementAdvance := 0;
end;

procedure TBLGlyphRun.SetFlags(const AValue: TBLGlyphRunFlags);
begin
  FHandle.flags := Cardinal(AValue);
end;

procedure TBLGlyphRun.SetGlyphData(const AData: Pointer;
  const AAdvance: Integer);
begin
  FHandle.glyphData := AData;
  FHandle.glyphAdvance := AAdvance;
end;

procedure TBLGlyphRun.SetGlyphData(const AData: PCardinal);
begin
  FHandle.glyphData := AData;
  FHandle.glyphAdvance := SizeOf(Cardinal);
end;

procedure TBLGlyphRun.SetGlyphData(const AData: PWord);
begin
  FHandle.glyphData := AData;
  FHandle.glyphAdvance := SizeOf(Word);
end;

procedure TBLGlyphRun.SetPlacementData(const AData: Pointer;
  const AAdvance: Integer);
begin
  FHandle.placementData := AData;
  FHandle.placementAdvance := AAdvance;
end;

procedure TBLGlyphRun.SetPlacementType(const AValue: TBLGlyphPlacementType);
begin
  FHandle.placementType := Ord(AValue);
end;

procedure TBLGlyphRun.SetSize(const AValue: Integer);
begin
  FHandle.size := AValue;
end;

{ TBLGlyphRun.TEnumerable<T> }

constructor TBLGlyphRun.TEnumerable<T>.Create(const AHandle: _PBLGlyphRun);
begin
  FHandle := AHandle;
end;

function TBLGlyphRun.TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TEnumerator<T>.Create(FHandle);
end;

{ TBLGlyphRun.TEnumerator<T> }

constructor TBLGlyphRun.TEnumerator<T>.Create(const AHandle: _PBLGlyphRun);
begin
  FGlyphData := AHandle.glyphData;
  FPlacementData := AHandle.placementData;
  FGlyphAdvance := AHandle.glyphAdvance;
  FPlacementAdvance := AHandle.placementAdvance;
  FHigh := AHandle.size;
  Dec(FHigh);
  FIndex := -1;
  Dec(FGlyphData, FGlyphAdvance);
  Dec(FPlacementData, FPlacementAdvance);
end;

function TBLGlyphRun.TEnumerator<T>.GetCurrent: TBLGlyphRunEntry<T>;
begin
  Result.GlyphId := PWord(FGlyphData)^;
  Result.Placement := P(FPlacementData)^;
end;

function TBLGlyphRun.TEnumerator<T>.MoveNext: Boolean;
begin
  Result := (FIndex < FHigh);
  if Result then
  begin
    Inc(FIndex);
    Inc(FGlyphData, FGlyphAdvance);
    Inc(FPlacementData, FPlacementAdvance);
  end;
end;

{ TBLFontFaceInfo }

function TBLFontFaceInfo.GetDiagFlags: TBLFontFaceDiagFlags;
begin
  Word(Result) := FHandle.diagFlags;
end;

function TBLFontFaceInfo.GetFaceFlags: TBLFontFaceFlags;
begin
  Cardinal(Result) := FHandle.faceFlags;
end;

function TBLFontFaceInfo.GetFaceType: TBLFontFaceType;
begin
  Result := TBLFontFaceType(FHandle.faceType);
end;

function TBLFontFaceInfo.GetGlyphCount: Integer;
begin
  Result := FHandle.glyphCount;
end;

function TBLFontFaceInfo.GetOutlineType: TBLFontOutlineType;
begin
  Result := TBLFontOutlineType(FHandle.outlineType);
end;

procedure TBLFontFaceInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFontFaceInfo.SetDiagFlags(const AValue: TBLFontFaceDiagFlags);
begin
  FHandle.diagFlags := Word(AValue);
end;

procedure TBLFontFaceInfo.SetFaceFlags(const AValue: TBLFontFaceFlags);
begin
  FHandle.faceFlags := Cardinal(AValue);
end;

procedure TBLFontFaceInfo.SetFaceType(const AValue: TBLFontFaceType);
begin
  FHandle.faceType := Ord(AValue);
end;

procedure TBLFontFaceInfo.SetGlyphCount(const AValue: Integer);
begin
  FHandle.glyphCount := AValue;
end;

procedure TBLFontFaceInfo.SetOutlineType(const AValue: TBLFontOutlineType);
begin
  FHandle.outlineType := Ord(AValue);
end;

{ TBLFontQueryProperties }

function TBLFontQueryProperties.GetStretch: TBLFontStretch;
begin
  Result := TBLFontStretch(FHandle.stretch);
end;

function TBLFontQueryProperties.GetStyle: TBLFontStyle;
begin
  Result := TBLFontStyle(FHandle.style);
end;

function TBLFontQueryProperties.GetWeight: TBLFontWeight;
begin
  Result := TBLFontWeight(FHandle.weight);
end;

procedure TBLFontQueryProperties.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFontQueryProperties.SetStretch(const AValue: TBLFontStretch);
begin
  FHandle.stretch := Ord(AValue);
end;

procedure TBLFontQueryProperties.SetStyle(const AValue: TBLFontStyle);
begin
  FHandle.style := Ord(AValue);
end;

procedure TBLFontQueryProperties.SetWeight(const AValue: TBLFontWeight);
begin
  FHandle.weight := Ord(AValue);
end;

{ TBLFontTable }

function TBLFontTable.GetSize: Integer;
begin
  Result := FHandle.size;
end;

procedure TBLFontTable.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFontTable.Reset(const AData: Pointer; const ASize: Integer);
begin
  FHandle.data := AData;
  FHandle.size := ASize;
end;

procedure TBLFontTable.SetSize(const AValue: Integer);
begin
  FHandle.size := AValue;
end;

{ TBLFontFeature }

procedure TBLFontFeature.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLFontVariation }

procedure TBLFontVariation.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLFontUnicodeCoverage }

procedure TBLFontUnicodeCoverage.ClearBit(
  const AIndex: TBLFontUnicodeCoverageIndex);
var
  I: Integer;
begin
  I := Ord(AIndex);
  FHandle.data[I shr 5] := FHandle.data[I shr 5] and not (1 shl (I and 31));
end;

class operator TBLFontUnicodeCoverage.Equal(const ALeft,
  ARight: TBLFontUnicodeCoverage): Boolean;
begin
  Result := (ALeft.FHandle.data[0] = ARight.FHandle.data[0])
        and (ALeft.FHandle.data[1] = ARight.FHandle.data[1])
        and (ALeft.FHandle.data[2] = ARight.FHandle.data[2])
        and (ALeft.FHandle.data[3] = ARight.FHandle.data[3]);
end;

function TBLFontUnicodeCoverage.GetBit(
  const AIndex: TBLFontUnicodeCoverageIndex): Boolean;
var
  I: Integer;
begin
  I := Ord(AIndex);
  Result := (((FHandle.data[I shr 5] shr (I and 31)) and 1) <> 0);
end;

function TBLFontUnicodeCoverage.GetIsEmpty: Boolean;
begin
  Result := (FHandle.data[0] = 0) and (FHandle.data[1] = 0)
        and (FHandle.data[2] = 0) and (FHandle.data[3] = 0);
end;

class operator TBLFontUnicodeCoverage.NotEqual(const ALeft,
  ARight: TBLFontUnicodeCoverage): Boolean;
begin
  Result := not (ALeft = ARight);
end;

procedure TBLFontUnicodeCoverage.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFontUnicodeCoverage.SetBit(
  const AIndex: TBLFontUnicodeCoverageIndex; const AValue: Boolean);
begin
  if (AValue) then
    SetBit(AIndex)
  else
    ClearBit(AIndex);
end;

procedure TBLFontUnicodeCoverage.SetBit(
  const AIndex: TBLFontUnicodeCoverageIndex);
var
  I: Integer;
begin
  I := Ord(AIndex);
  FHandle.data[I shr 5] := FHandle.data[I shr 5] or (1 shl (I and 31));
end;

{ TBLFontPanose }

function TBLFontPanose.GetIsEmpty: Boolean;
begin
  Result := (Data[0] = 0) and (Data[1] = 0)
        and (Data[2] = 0) and (Data[3] = 0)
        and (Data[4] = 0) and (Data[5] = 0)
        and (Data[6] = 0) and (Data[7] = 0)
        and (Data[8] = 0) and (Data[9] = 0);
end;

procedure TBLFontPanose.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TBLFontMatrix }

function BLFontMatrix(const AM00, AM01, AM10, AM11: Double): TBLFontMatrix; inline;
begin
  Result.Reset(AM00, AM01, AM10, AM11);
end;

function TBLFontMatrix.GetElement(const AIndex: Integer): Double;
begin
  Assert(Cardinal(AIndex) < 4);
  Result := FHandle.m[AIndex];
end;

procedure TBLFontMatrix.Reset;
begin
  FHandle.m00 := 1;
  FHandle.m01 := 0;
  FHandle.m10 := 0;
  FHandle.m11 := 1;
end;

procedure TBLFontMatrix.Reset(const AM00, AM01, AM10, AM11: Double);
begin
  FHandle.m00 := AM00;
  FHandle.m01 := AM01;
  FHandle.m10 := AM10;
  FHandle.m11 := AM11;
end;

procedure TBLFontMatrix.SetElement(const AIndex: Integer; const AValue: Double);
begin
  Assert(Cardinal(AIndex) < 4);
  FHandle.m[AIndex] := AValue;
end;

{ TBLFontMetrics }

procedure TBLFontMetrics.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

{ TBLFontDesignMetrics }

function TBLFontDesignMetrics.GetGlyphBoundingBox: TBLBoxI;
begin
  Result.FHandle := FHandle.glyphBoundingBox;
end;

procedure TBLFontDesignMetrics.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLFontDesignMetrics.SetGlyphBoundingBox(const AValue: TBLBoxI);
begin
  FHandle.glyphBoundingBox := AValue.FHandle;
end;

{ TBLTextMetrics }

function TBLTextMetrics.GetAdvance: TBLPoint;
begin
  Result.FHandle := FHandle.advance;
end;

function TBLTextMetrics.GetBoundingBox: TBLBox;
begin
  Result.FHandle := FHandle.boundingBox;
end;

function TBLTextMetrics.GetLeadingBearing: TBLPoint;
begin
  Result.FHandle := FHandle.leadingBearing;
end;

function TBLTextMetrics.GetTrailingBearing: TBLPoint;
begin
  Result.FHandle := FHandle.trailingBearing;
end;

procedure TBLTextMetrics.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLTextMetrics.SetAdvance(const AValue: TBLPoint);
begin
  FHandle.advance := AValue.FHandle;
end;

procedure TBLTextMetrics.SetBoundingBox(const AValue: TBLBox);
begin
  FHandle.boundingBox := AValue.FHandle;
end;

procedure TBLTextMetrics.SetLeadingBearing(const AValue: TBLPoint);
begin
  FHandle.leadingBearing := AValue.FHandle;
end;

procedure TBLTextMetrics.SetTrailingBearing(const AValue: TBLPoint);
begin
  FHandle.trailingBearing := AValue.FHandle;
end;

{$ENDREGION 'Font Defs'}

{$REGION 'Glyph Buffer'}

{ TBLGlyphBuffer }

procedure TBLGlyphBuffer.Clear;
begin
  _BLCheck(blGlyphBufferClear(@FHandle));
end;

constructor TBLGlyphBuffer.Create;
begin
  inherited;
  blGlyphBufferInit(@FHandle);
end;

destructor TBLGlyphBuffer.Destroy;
begin
  blGlyphBufferDestroy(@FHandle);
  inherited;
end;

function TBLGlyphBuffer.GetContent: PCardinal;
begin
  Result := PCardinal(FHandle.impl.data.content);
end;

function TBLGlyphBuffer.GetFlags: TBLGlyphRunFlags;
begin
  Cardinal(Result) := FHandle.impl.data.flags;
end;

function TBLGlyphBuffer.GetGlyphRun: PBLGlyphRun;
begin
  Result := @FHandle.impl.data.glyphRun;
end;

function TBLGlyphBuffer.GetHandle: PBLGlyphBufferCore;
begin
  Result := @FHandle;
end;

function TBLGlyphBuffer.GetHasGlyphs: Boolean;
begin
  Result := ((FHandle.impl.data.flags and BL_GLYPH_RUN_FLAG_UCS4_CONTENT) = 0);
end;

function TBLGlyphBuffer.GetHasInvalidChars: Boolean;
begin
  Result := ((FHandle.impl.data.flags and BL_GLYPH_RUN_FLAG_INVALID_TEXT) <> 0);
end;

function TBLGlyphBuffer.GetHasInvalidFontData: Boolean;
begin
  Result := ((FHandle.impl.data.flags and BL_GLYPH_RUN_FLAG_INVALID_FONT_DATA) <> 0);
end;

function TBLGlyphBuffer.GetHasText: Boolean;
begin
  Result := ((FHandle.impl.data.flags and BL_GLYPH_RUN_FLAG_UCS4_CONTENT) <> 0);
end;

function TBLGlyphBuffer.GetHasUndefinedChars: Boolean;
begin
  Result := ((FHandle.impl.data.flags and BL_GLYPH_RUN_FLAG_UNDEFINED_GLYPHS) <> 0);
end;

function TBLGlyphBuffer.GetInfoData: PBLGlyphInfo;
begin
  Result := PBLGlyphInfo(FHandle.impl.infoData);
end;

function TBLGlyphBuffer.GetIsEmpty: Boolean;
begin
  Result := (FHandle.impl.data.glyphRun.size = 0);
end;

function TBLGlyphBuffer.GetPlacementData: PBLGlyphPlacement;
begin
  Result := PBLGlyphPlacement(FHandle.impl.data.placementData);
end;

function TBLGlyphBuffer.GetSize: Integer;
begin
  Result := FHandle.impl.data.size;
end;

function TBLGlyphBuffer.HasFlag(const AFlag: TBLGlyphRunFlag): Boolean;
begin
  Result := ((FHandle.impl.data.flags and Ord(AFlag)) <> 0);
end;

procedure TBLGlyphBuffer.Reset;
begin
  _BLCheck(blGlyphBufferReset(@FHandle));
end;

procedure TBLGlyphBuffer.SetGlyphs(const AGlyphData: Pointer; const ASize,
  AGlyphIdSize, AGlyphAdvance: Integer);
begin
  _BLCheck(blGlyphBufferSetGlyphsFromStruct(@FHandle, AGlyphData, ASize,
    AGlyphIdSize, AGlyphAdvance));
end;

procedure TBLGlyphBuffer.SetGlyphs(const AGlyphData: PCardinal;
  const ALength: Integer);
begin
  _BLCheck(blGlyphBufferSetGlyphs(@FHandle, Pointer(AGlyphData), ALength));
end;

procedure TBLGlyphBuffer.SetGlyphs(const AGlyphData: TArray<Cardinal>);
begin
  _BLCheck(blGlyphBufferSetGlyphs(@FHandle, Pointer(AGlyphData), Length(AGlyphData)));
end;

procedure TBLGlyphBuffer.SetText(const AText: String);
begin
  _BLCheck(blGlyphBufferSetText(@FHandle, PChar(AText), Length(AText), BL_TEXT_ENCODING_UTF16));
end;

procedure TBLGlyphBuffer.SetText(const AText: Pointer; const ALength: Integer;
  const AEncoding: TBLTextEncoding);
begin
  _BLCheck(blGlyphBufferSetText(@FHandle, AText, ALength, Ord(AEncoding)));
end;

procedure TBLGlyphBuffer.SetText(const AText: UCS4String);
begin
  _BLCheck(blGlyphBufferSetText(@FHandle, Pointer(AText), Length(AText) - 1, BL_TEXT_ENCODING_UTF32));
end;

procedure TBLGlyphBuffer.SetText(const AText: UTF8String);
begin
  _BLCheck(blGlyphBufferSetText(@FHandle, PUTF8Char(AText), Length(AText), BL_TEXT_ENCODING_UTF8));
end;

{$ENDREGION 'Glyph Buffer'}

{$REGION 'Font'}

{ TBLFontData }

constructor TBLFontData.Create;
begin
  inherited;
  blFontDataInit(@FHandle);
end;

constructor TBLFontData.Create(const AHandle: BLFontDataCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
end;

destructor TBLFontData.Destroy;
begin
  if (not FIsReference) then
    blFontDataDestroy(@FHandle);
  inherited;
end;

class procedure TBLFontData.DoDestroy(impl, destroyData: Pointer);
var
  Data: PDestroyData absolute destroyData;
begin
  if (Data <> nil) then
  begin
    Data.Event(Data.FontData);
    FreeMem(Data);
  end;
end;

function TBLFontData.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLFontData) then
    Result := blFontDataEquals(@FHandle, @TBLFontData(Obj).FHandle)
  else
    Result := False;
end;

function TBLFontData.Equals(const AOther: IBLFontData): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blFontDataEquals(@FHandle, AOther.Handle)
end;

function TBLFontData.GetFaceCount: Integer;
begin
  Result := FHandle.impl.faceCount;
end;

function TBLFontData.GetFaceType: TBLFontFaceType;
begin
  Result := TBLFontFaceType(FHandle.impl.faceType);
end;

function TBLFontData.GetFlags: TBLFontDataFlags;
begin
  Byte(Result) := FHandle.impl.flags;
end;

function TBLFontData.GetHandle: PBLFontDataCore;
begin
  Result := @FHandle;
end;

function TBLFontData.GetIsCollection: Boolean;
begin
  Result := ((FHandle.impl.flags and BL_FONT_DATA_FLAG_COLLECTION) <> 0);
end;

function TBLFontData.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLFontData.GetTags(const AFaceIndex: Integer): TArray<TBLTag>;
var
  Tags: IBLArray;
begin
  Tags := TBLUtils.CreateBLArray<TBLTag>;
  _BLCheck(blFontDataListTags(@FHandle, AFaceIndex, Tags.Handle));
  Result := TBLUtils.BLArrayToArray<TBLTag>(Tags);
end;

procedure TBLFontData.InitializeFromData(const AData: TBytes);
var
  Data: IBLArray;
begin
  if (AData <> nil) then
  begin
    Data := TBLUtils.ArrayToBLArray<Byte>(AData);
    _BLCheck(blFontDataCreateFromDataArray(@FHandle, Data.Handle));
  end;
end;

procedure TBLFontData.InitializeFromData(const AData: Pointer;
  const ASize: Integer; const AOnDestroy: TBLFontDataDestroyEvent);
var
  DestroyFunc: BLDestroyImplFunc;
  DestroyData: PDestroyData;
begin
  if Assigned(AOnDestroy) then
  begin
    DestroyFunc := DoDestroy;
    GetMem(DestroyData, SizeOf(TDestroyData));
    DestroyData.FontData := Self;
    DestroyData.Event := AOnDestroy;
  end
  else
  begin
    DestroyFunc := nil;
    DestroyData := nil;
  end;
  _BLCheck(blFontDataCreateFromData(@FHandle, AData, ASize, DestroyFunc, DestroyData));
end;

procedure TBLFontData.InitializeFromFile(const AFilename: String;
  const AReadFlags: TBLFileReadFlags);
begin
  _BLCheck(blFontDataCreateFromFile(@FHandle, MarshaledAString(UTF8String(AFilename)),
    Byte(AReadFlags)));
end;

function TBLFontData.QueryTable(const AFaceIndex: Integer; const ATag: TBLTag;
  out ATable: TBLFontTable): Integer;
begin
  Result := blFontDataQueryTables(@FHandle, AFaceIndex, @ATable.FHandle, @ATag, 1);
end;

function TBLFontData.QueryTables(const AFaceIndex: Integer;
  const ATags: TArray<TBLTag>; out ATables: TArray<TBLFontTable>): Integer;
begin
  SetLength(ATables, Length(ATags));
  Result := blFontDataQueryTables(@FHandle, AFaceIndex, Pointer(ATables),
    Pointer(ATags), Length(ATags));
end;

procedure TBLFontData.Reset;
begin
  _BLCheck(blFontDataReset(@FHandle));
end;

{ TBLFontFace }

constructor TBLFontFace.Create;
begin
  inherited;
  blFontFaceInit(@FHandle);
end;

constructor TBLFontFace.Create(const AHandle: BLFontFaceCore;
  const AIsReference: Boolean);
begin
  inherited Create;
  FHandle := AHandle;
  FIsReference := AIsReference;
end;

destructor TBLFontFace.Destroy;
begin
  if (not FIsReference) then
    blFontFaceDestroy(@FHandle);
  inherited;
end;

function TBLFontFace.Equals(const AOther: IBLFontFace): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blFontFaceEquals(@FHandle, AOther.Handle)
end;

function TBLFontFace.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLFontFace) then
    Result := blFontFaceEquals(@FHandle, @TBLFontFace(Obj).FHandle)
  else
    Result := False;
end;

function TBLFontFace.GetData: IBLFontData;
begin
  if (FData = nil) then
    FData := TBLFontData.Create(FHandle.impl.data, True);

  Result := FData;
end;

function TBLFontFace.GetDesignMetrics: TBLFontDesignMetrics;
begin
  Result.FHandle := FHandle.impl.designMetrics;
end;

function TBLFontFace.GetDiagFlags: TBLFontFaceDiagFlags;
begin
  Word(Result) := FHandle.impl.faceInfo.diagFlags;
end;

function TBLFontFace.GetFaceFlags: TBLFontFaceFlags;
begin
  Cardinal(Result) := FHandle.impl.faceInfo.faceFlags;
end;

function TBLFontFace.GetFaceIndex: Integer;
begin
  Result := FHandle.impl.faceInfo.faceIndex;
end;

function TBLFontFace.GetFaceInfo: TBLFontFaceInfo;
begin
  Result.FHandle := FHandle.impl.faceInfo;
end;

function TBLFontFace.GetFaceType: TBLFontFaceType;
begin
  Result := TBLFontFaceType(FHandle.impl.faceInfo.faceType);
end;

function TBLFontFace.GetFamilyName: String;
var
  S: UTF8String;
begin
  SetString(S, FHandle.impl.familyName.impl.data, FHandle.impl.familyName.impl.size);
  Result := String(S);
end;

function TBLFontFace.GetFullName: String;
var
  S: UTF8String;
begin
  SetString(S, FHandle.impl.fullName.impl.data, FHandle.impl.fullName.impl.size);
  Result := String(S);
end;

function TBLFontFace.GetHandle: PBLFontFaceCore;
begin
  Result := @FHandle;
end;

function TBLFontFace.GetHasBaselineYAt0: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_BASELINE_Y_EQUALS_0) <> 0);
end;

function TBLFontFace.GetHasCharToGlyphMapping: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_CHAR_TO_GLYPH_MAPPING) <> 0);
end;

function TBLFontFace.GetHasHorizontalKerning: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_HORIZONTAL_KERNING) <> 0);
end;

function TBLFontFace.GetHasHorizontalMetrics: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_HORIZONTAL_METRICS) <> 0);
end;

function TBLFontFace.GetHasLSBPointXAt0: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_LSB_POINT_X_EQUALS_0) <> 0);
end;

function TBLFontFace.GetHasOpenTypeFeatures: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_OPENTYPE_FEATURES) <> 0);
end;

function TBLFontFace.GetHasOpenTypeVariations: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_OPENTYPE_VARIATIONS) <> 0);
end;

function TBLFontFace.GetHasPanoseData: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_PANOSE_DATA) <> 0);
end;

function TBLFontFace.GetHasTypographicMetrics: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_TYPOGRAPHIC_METRICS) <> 0);
end;

function TBLFontFace.GetHasTypographicNames: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_TYPOGRAPHIC_NAMES) <> 0);
end;

function TBLFontFace.GetHasUnicodeCoverage: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_UNICODE_COVERAGE) <> 0);
end;

function TBLFontFace.GetHasVariationSequences: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_VARIATION_SEQUENCES) <> 0);
end;

function TBLFontFace.GetHasVerticalKerning: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_VERTICAL_KERNING) <> 0);
end;

function TBLFontFace.GetHasVerticalMetrics: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_VERTICAL_METRICS) <> 0);
end;

function TBLFontFace.GetIsLastResortFont: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_LAST_RESORT_FONT) <> 0);
end;

function TBLFontFace.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLFontFace.GetIsSymbolFont: Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and BL_FONT_FACE_FLAG_SYMBOL_FONT) <> 0);
end;

function TBLFontFace.GetOutlineType: TBLFontOutlineType;
begin
  Result := TBLFontOutlineType(FHandle.impl.faceInfo.outlineType);
end;

function TBLFontFace.GetPanose: TBLFontPanose;
begin
  Result := TBLFontPanose(FHandle.impl.panose);
end;

function TBLFontFace.GetPostScriptName: String;
var
  S: UTF8String;
begin
  SetString(S, FHandle.impl.postScriptName.impl.data, FHandle.impl.postScriptName.impl.size);
  Result := String(S);
end;

function TBLFontFace.GetStretch: TBLFontStretch;
begin
  Result := TBLFontStretch(FHandle.impl.stretch);
end;

function TBLFontFace.GetStyle: TBLFontStyle;
begin
  Result := TBLFontStyle(FHandle.impl.style);
end;

function TBLFontFace.GetSubfamilyName: String;
var
  S: UTF8String;
begin
  SetString(S, FHandle.impl.subfamilyName.impl.data, FHandle.impl.subfamilyName.impl.size);
  Result := String(S);
end;

function TBLFontFace.GetUnicodeCoverage: TBLFontUnicodeCoverage;
begin
  Result.FHandle := FHandle.impl.unicodeCoverage;
end;

function TBLFontFace.GetUniqueId: TBLUniqueId;
begin
  Result := FHandle.impl.uniqueId;
end;

function TBLFontFace.GetUnitsPerEm: Integer;
begin
  Result := FHandle.impl.designMetrics.unitsPerEm;
end;

function TBLFontFace.GetWeight: TBLFontWeight;
begin
  Result := TBLFontWeight(FHandle.impl.weight);
end;

function TBLFontFace.HasFaceFlag(const AFlag: TBLFontFaceFlag): Boolean;
begin
  Result := ((FHandle.impl.faceInfo.faceFlags and Ord(AFlag)) <> 0);
end;

procedure TBLFontFace.InitializeFromData(const AFontData: IBLFontData;
  const AFaceIndex: Integer);
begin
  FData := AFontData;
  if (AFontData <> nil) then
    _BLCheck(blFontFaceCreateFromData(@FHandle, AFontData.Handle, AFaceIndex));
end;

procedure TBLFontFace.InitializeFromFile(const AFilename: String;
  const AReadFlags: TBLFileReadFlags);
begin
  _BLCheck(blFontFaceCreateFromFile(@FHandle, MarshaledAString(UTF8String(AFilename)),
    Byte(AReadFlags)));
end;

procedure TBLFontFace.Reset;
begin
  _BLCheck(blFontFaceReset(@FHandle));
end;

{ TBLFont }

procedure TBLFont.ApplyGPos(const AGlyphBuffer: IBLGlyphBuffer;
  const AIndex: Integer; const ALookups: TBLBitWord);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontApplyGPos(@FHandle, AGlyphBuffer.Handle, AIndex, ALookups));
end;

procedure TBLFont.ApplyGSub(const AGlyphBuffer: IBLGlyphBuffer;
  const AIndex: Integer; const ALookups: TBLBitWord);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontApplyGSub(@FHandle, AGlyphBuffer.Handle, AIndex, ALookups));
end;

procedure TBLFont.ApplyKerning(const AGlyphBuffer: IBLGlyphBuffer);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontApplyKerning(@FHandle, AGlyphBuffer.Handle));
end;

constructor TBLFont.Create;
begin
  inherited;
  blFontInit(@FHandle);
end;

destructor TBLFont.Destroy;
begin
  blFontDestroy(@FHandle);
  inherited;
end;

class function TBLFont.DoSink(path: PBLPathCore; info,
  closure: Pointer): Cardinal;
var
  Font: TBLFont;
begin
  Result := 0;
  if Assigned(closure) then
  begin
    Font := TBLFont(Closure);
    if Assigned(Font.FSink) then
      Result := Ord(Font.FSink(Font.FSinkPath, info));
  end;
end;

function TBLFont.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLFont) then
    Result := blFontEquals(@FHandle, @TBLFont(Obj).FHandle)
  else
    Result := False;
end;

function TBLFont.Equals(const AOther: IBLFont): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blFontEquals(@FHandle, AOther.Handle)
end;

function TBLFont.GetDesignMetrics: TBLFontDesignMetrics;
begin
  Result.FHandle := FHandle.impl.face.impl.designMetrics;
end;

function TBLFont.GetFace: IBLFontFace;
begin
  Result := FFace;
end;

function TBLFont.GetFaceFlags: TBLFontFaceFlags;
begin
  Result := TBLFontFaceFlags(FHandle.impl.face.impl.faceInfo.faceFlags);
end;

function TBLFont.GetFaceType: TBLFontFaceType;
begin
  Result := TBLFontFaceType(FHandle.impl.face.impl.faceInfo.faceType);
end;

function TBLFont.GetFeatures: TArray<TBLFontFeature>;
begin
  Result := TBLUtils.BLArrayToArray<TBLFontFeature>(FHandle.impl.features);
end;

function TBLFont.GetGlyphAdvances(const AGlyphData: PCardinal;
  const AGlyphAdvance, ACount: Integer): TArray<TBLGlyphPlacement>;
begin
  SetLength(Result, ACount);
  _BLCheck(blFontGetGlyphAdvances(@FHandle, Pointer(AGlyphData), AGlyphAdvance,
    Pointer(Result), ACount));
end;

function TBLFont.GetGlyphBounds(const AGlyphData: PCardinal;
  const AGlyphAdvance, ACount: Integer): TArray<TBLBoxI>;
begin
  SetLength(Result, ACount);
  _BLCheck(blFontGetGlyphBounds(@FHandle, Pointer(AGlyphData), AGlyphAdvance,
    Pointer(Result), ACount));
end;

function TBLFont.GetGlyphOutlines(const AGlyphId: Cardinal;
  const AUserMatrix: TBLMatrix2D; const ASink: TBLPathSinkEvent): IBLPath;
var
  Sink: BLPathSinkFunc;
  Closure: Pointer;
begin
  Result := TBLPath.Create;
  if Assigned(ASink) then
  begin
    FSink := ASink;
    FSinkPath := Result;
    Sink := DoSink;
    Closure := Self;
  end
  else
  begin
    Sink := nil;
    Closure := nil;
  end;
  _BLCheck(blFontGetGlyphOutlines(@FHandle, AGlyphId, @AUserMatrix,
    Result.Handle, Sink, Closure));
end;

function TBLFont.GetGlyphOutlines(const AGlyphId: Cardinal;
  const ASink: TBLPathSinkEvent): IBLPath;
var
  Sink: BLPathSinkFunc;
  Closure: Pointer;
begin
  Result := TBLPath.Create;
  if Assigned(ASink) then
  begin
    FSink := ASink;
    FSinkPath := Result;
    Sink := DoSink;
    Closure := Self;
  end
  else
  begin
    Sink := nil;
    Closure := nil;
  end;
  _BLCheck(blFontGetGlyphOutlines(@FHandle, AGlyphId, nil,
    Result.Handle, Sink, Closure));
end;

function TBLFont.GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
  const AUserMatrix: TBLMatrix2D; const ASink: TBLPathSinkEvent): IBLPath;
var
  Sink: BLPathSinkFunc;
  Closure: Pointer;
begin
  Result := TBLPath.Create;
  if Assigned(ASink) then
  begin
    FSink := ASink;
    FSinkPath := Result;
    Sink := DoSink;
    Closure := Self;
  end
  else
  begin
    Sink := nil;
    Closure := nil;
  end;
  _BLCheck(blFontGetGlyphRunOutlines(@FHandle, @AGlyphRun, @AUserMatrix,
    Result.Handle, Sink, Closure));
end;

function TBLFont.GetGlyphRunOutlines(const AGlyphRun: TBLGlyphRun;
  const ASink: TBLPathSinkEvent): IBLPath;
var
  Sink: BLPathSinkFunc;
  Closure: Pointer;
begin
  Result := TBLPath.Create;
  if Assigned(ASink) then
  begin
    FSink := ASink;
    FSinkPath := Result;
    Sink := DoSink;
    Closure := Self;
  end
  else
  begin
    Sink := nil;
    Closure := nil;
  end;
  _BLCheck(blFontGetGlyphRunOutlines(@FHandle, @AGlyphRun, nil,
    Result.Handle, Sink, Closure));
end;

function TBLFont.GetHandle: PBLFontCore;
begin
  Result := @FHandle;
end;

function TBLFont.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLFont.GetMatrix: TBLFontMatrix;
begin
  Result.FHandle := FHandle.impl.matrix;
end;

function TBLFont.GetMetrics: TBLFontMetrics;
begin
  Result.FHandle := FHandle.impl.metrics;
end;

function TBLFont.GetMetricsPtr: PBLFontMetrics;
begin
  Result := @FHandle.impl.metrics;
end;

function TBLFont.GetSize: Single;
begin
  Result := FHandle.impl.metrics.size;
end;

function TBLFont.GetStretch: TBLFontStretch;
begin
  Result := TBLFontStretch(FHandle.impl.stretch);
end;

function TBLFont.GetStyle: TBLFontStyle;
begin
  Result := TBLFontStyle(FHandle.impl.style);
end;

function TBLFont.GetTextMetrics(
  const AGlyphBuffer: IBLGlyphBuffer): TBLTextMetrics;
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontGetTextMetrics(@FHandle, AGlyphBuffer.Handle, @Result.FHandle));
end;

function TBLFont.GetUnitsPerEm: Integer;
begin
  Result := FHandle.impl.face.impl.designMetrics.unitsPerEm;
end;

function TBLFont.GetVariations: TArray<TBLFontVariation>;
begin
  Result := TBLUtils.BLArrayToArray<TBLFontVariation>(FHandle.impl.variations);
end;

function TBLFont.GetWeight: TBLFontWeight;
begin
  Result := TBLFontWeight(FHandle.impl.weight);
end;

procedure TBLFont.InitializeFromFace(const AFace: IBLFontFace;
  const ASize: Single);
begin
  if (AFace <> nil) then
    _BLCheck(blFontCreateFromFace(@FHandle, AFace.Handle, ASize));
end;

procedure TBLFont.MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontMapTextToGlyphs(@FHandle, AGlyphBuffer.Handle, nil));
end;

procedure TBLFont.MapTextToGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
  out AState: TBLGlyphMappingState);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontMapTextToGlyphs(@FHandle, AGlyphBuffer.Handle, @AState.FHandle));
end;

procedure TBLFont.PositionGlyphs(const AGlyphBuffer: IBLGlyphBuffer;
  const APositioningFlags: Cardinal);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontPositionGlyphs(@FHandle, AGlyphBuffer.Handle, APositioningFlags));
end;

procedure TBLFont.Reset;
begin
  _BLCheck(blFontReset(@FHandle));
end;

procedure TBLFont.Shape(const AGlyphBuffer: IBLGlyphBuffer);
begin
  if (AGlyphBuffer <> nil) then
    _BLCheck(blFontShape(@FHandle, AGlyphBuffer.Handle));
end;

{$ENDREGION 'Font'}

{$REGION 'Font Manager'}

{ TBLFontManager }

procedure TBLFontManager.AddFace(const AFace: IBLFontFace);
begin
  if (AFace <> nil) then
    _BLCheck(blFontManagerAddFace(@FHandle, AFace.Handle));
end;

constructor TBLFontManager.Create;
begin
  inherited;
  _BLCheck(blFontManagerInit(@FHandle));
end;

destructor TBLFontManager.Destroy;
begin
  blFontManagerDestroy(@FHandle);
  inherited;
end;

function TBLFontManager.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLFontManager) then
    Result := blFontManagerEquals(@FHandle, @TBLFontManager(Obj).FHandle)
  else
    Result := False;
end;

function TBLFontManager.Equals(const AOther: IBLFontManager): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := blFontManagerEquals(@FHandle, AOther.Handle);
end;

function TBLFontManager.GetFaceCount: Integer;
begin
  Result := blFontManagerGetFaceCount(@FHandle);
end;

function TBLFontManager.GetFamilyCount: Integer;
begin
  Result := blFontManagerGetFamilyCount(@FHandle);
end;

function TBLFontManager.GetHandle: PBLFontManagerCore;
begin
  Result := @FHandle;
end;

function TBLFontManager.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLFontManager.HasFace(const AFace: IBLFontFace): Boolean;
begin
  if (AFace <> nil) then
    Result := blFontManagerHasFace(@FHandle, AFace.Handle)
  else
    Result := False;
end;

procedure TBLFontManager.Initialize;
begin
  blFontManagerCreate(@FHandle);
end;

function TBLFontManager.QueryFace(const AName: String): IBLFontFace;
var
  Name: UTF8String;
  Face: BLFontFaceCore;
begin
  Name := UTF8String(AName);
  if (blFontManagerQueryFace(@FHandle, MarshaledAString(Name), Length(Name),
    nil, @Face) = BL_SUCCESS)
  then
    Result := TBLFontFace.Create(Face, True)
  else
    Result := nil;
end;

function TBLFontManager.QueryFace(const AName: String;
  const AProperties: TBLFontQueryProperties): IBLFontFace;
var
  Name: UTF8String;
  Face: BLFontFaceCore;
begin
  Name := UTF8String(AName);
  if (blFontManagerQueryFace(@FHandle, MarshaledAString(Name), Length(Name),
    @AProperties.FHandle, @Face) = BL_SUCCESS)
  then
    Result := TBLFontFace.Create(Face, True)
  else
    Result := nil;
end;

function TBLFontManager.QueryFacesByFamilyName(
  const AName: String): TArray<IBLFontFace>;
var
  Name: UTF8String;
  FaceArray: IBLArray;
  FaceHandles: TArray<BLFontFaceCore>;
  I: Integer;
begin
  Name := UTF8String(AName);
  FaceArray := TBLUtils.CreateBLArray<BLFontFaceCore>;
  if (blFontManagerQueryFacesByFamilyName(@FHandle, MarshaledAString(Name),
    Length(Name), FaceArray.Handle) = BL_SUCCESS) then
  begin
    FaceHandles := TBLUtils.BLArrayToArray<BLFontFaceCore>(FaceArray);
    SetLength(Result, Length(FaceHandles));
    for I := 0 to Length(Result) - 1 do
      Result[I] := TBLFontFace.Create(FaceHandles[I], True);
  end
  else
    Result := nil;
end;

procedure TBLFontManager.Reset;
begin
  _BLCheck(blFontManagerReset(@FHandle));
end;

{$ENDREGION 'Font Manager'}

{$REGION 'Pixel Converter'}

{ TBLPixelConverterOptions }

function TBLPixelConverterOptions.GetGap: Integer;
begin
  Result := FHandle.gap;
end;

function TBLPixelConverterOptions.GetOrigin: TBLPointI;
begin
  Result.FHandle := FHandle.origin;
end;

procedure TBLPixelConverterOptions.SetGap(const AValue: Integer);
begin
  FHandle.gap := AValue;
end;

procedure TBLPixelConverterOptions.SetOrigin(const AValue: TBLPointI);
begin
  FHandle.origin := AValue.FHandle;
end;

{ TBLPixelConverter }

procedure TBLPixelConverter.Assign(const AOther: IBLPixelConverter);
begin
  if Assigned(AOther) then
    _BLCheck(blPixelConverterAssign(@FHandle, AOther.Handle));
end;

procedure TBLPixelConverter.ConvertRect(const ASrcData: Pointer;
  const ASrcStride: Integer; const ADstData: Pointer; const ADstStride, AWidth,
  AHeight: Integer; const AOptions: TBLPixelConverterOptions);
begin
  _BLCheck(blPixelConverterConvert(@FHandle, ADstData, ADstStride, ASrcData,
    ASrcStride, AWidth, AHeight, @AOptions.FHandle));
end;

procedure TBLPixelConverter.ConvertRect(const ASrcData: Pointer;
  const ASrcStride: Integer; const ADstData: Pointer; const ADstStride, AWidth,
  AHeight: Integer);
begin
  _BLCheck(blPixelConverterConvert(@FHandle, ADstData, ADstStride, ASrcData,
    ASrcStride, AWidth, AHeight, nil));
end;

procedure TBLPixelConverter.ConvertSpan(const ASrcData, ADstData: Pointer;
  const AWidth: Integer);
begin
  _BLCheck(blPixelConverterConvert(@FHandle, ADstData, 0, ASrcData, 0,
    AWidth, 1, nil));
end;

procedure TBLPixelConverter.ConvertSpan(const ASrcData, ADstData: Pointer;
  const AWidth: Integer; const AOptions: TBLPixelConverterOptions);
begin
  _BLCheck(blPixelConverterConvert(@FHandle, ADstData, 0, ASrcData, 0,
    AWidth, 1, @AOptions.FHandle));
end;

constructor TBLPixelConverter.Create;
begin
  inherited;
  blPixelConverterInit(@FHandle);
end;

class function TBLPixelConverter.CreatePlatformConverter: IBLPixelConverter;
var
  SrcFormat, DstFormat: TBLFormatInfo;
begin
  Result := TBLPixelConverter.Create;
  SrcFormat := TBLFormat.PRGB32.Info;
  DstFormat := SrcFormat;

  {$IFNDEF MSWINDOWS}
  DstFormat.RedShift := SrcFormat.BlueShift;
  DstFormat.BlueShift := SrcFormat.RedShift;
  {$ENDIF}

  Result.Initialize(SrcFormat, DstFormat, [TBLPixelConverterCreateFlag.NoMultiStep]);
end;

destructor TBLPixelConverter.Destroy;
begin
  blPixelConverterDestroy(@FHandle);
  inherited;
end;

function TBLPixelConverter.GetHandle: PBLPixelConverterCore;
begin
  Result := @FHandle;
end;

function TBLPixelConverter.GetIsInitialized: Boolean;
begin
  Result := (FHandle.internalFlags <> 0);
end;

procedure TBLPixelConverter.Initialize(const ASrcInfo, ADstInfo: TBLFormatInfo;
  const ACreateFlags: TBLPixelConverterCreateFlags);
begin
  _BLCheck(blPixelConverterCreate(@FHandle, @ADstInfo.FHandle,
    @ASrcInfo.FHandle, Byte(ACreateFlags)));
end;

procedure TBLPixelConverter.Reset;
begin
  _BLCheck(blPixelConverterReset(@FHandle));
end;

{$ENDREGION 'Pixel Converter'}

{$REGION 'Context'}

{ TBLContextCreateInfo }

function TBLContextCreateInfo.GetFlags: TBLContextCreateFlags;
begin
  Cardinal(Result) := FHandle.flags;
end;

procedure TBLContextCreateInfo.Reset;
begin
  FillChar(FHandle, SizeOf(FHandle), 0);
end;

procedure TBLContextCreateInfo.SetFlags(const AValue: TBLContextCreateFlags);
begin
  FHandle.flags := Cardinal(AValue);
end;

{ TBLContextCookie }

class operator TBLContextCookie.Equal(const ALeft,
  ARight: TBLContextCookie): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLContextCookie.Equals(const AOther: TBLContextCookie): Boolean;
begin
  Result := (FHandle.data[0] = AOther.FHandle.data[0])
        and (FHandle.data[1] = AOther.FHandle.data[1]);
end;

function TBLContextCookie.GetIsEmpty: Boolean;
begin
  Result := (FHandle.data[0] = 0) and (FHandle.data[1] = 0);
end;

class operator TBLContextCookie.NotEqual(const ALeft,
  ARight: TBLContextCookie): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLContextCookie.Reset(const AData0, AData1: UInt64);
begin
 FHandle.data[0] := AData0;
 FHandle.data[1] := AData1;
end;

procedure TBLContextCookie.Reset(const AOther: TBLContextCookie);
begin
  Self := AOther;
end;

procedure TBLContextCookie.Reset;
begin
  Reset(0, 0);
end;

{ TBLContextHints }

function TBLContextHints.GetGradientQuality: TBLGradientQuality;
begin
  Result := TBLGradientQuality(FHandle.gradientQuality);
end;

function TBLContextHints.GetPatternQuality: TBLPatternQuality;
begin
  Result := TBLPatternQuality(FHandle.patternQuality);
end;

function TBLContextHints.GetRenderingQuality: TBLRenderingQuality;
begin
  Result := TBLRenderingQuality(FHandle.renderingQuality);
end;

procedure TBLContextHints.SetGradientQuality(const AValue: TBLGradientQuality);
begin
  FHandle.gradientQuality := Ord(AValue);
end;

procedure TBLContextHints.SetPatternQuality(const AValue: TBLPatternQuality);
begin
  FHandle.patternQuality := Ord(AValue);
end;

procedure TBLContextHints.SetRenderingQuality(const AValue: TBLRenderingQuality);
begin
  FHandle.renderingQuality := Ord(AValue);
end;

{ TBLContextState }

function TBLContextState.GetApproximationOptions: TBLApproximationOptions;
begin
  Result.FHandle := FHandle.approximationOptions;
end;

function TBLContextState.GetCompOp: TBLCompOp;
begin
  Result := TBLCompOp(FHandle.compOp);
end;

function TBLContextState.GetFillAlpha: Double;
begin
  Result := FHandle.styleAlpha[BL_CONTEXT_OP_TYPE_FILL];
end;

function TBLContextState.GetFillRule: TBLFillRule;
begin
  Result := TBLFillRule(FHandle.fillRule);
end;

function TBLContextState.GetFillStyle: TBLStyleType;
begin
  Result := TBLStyleType(FHandle.styleType[BL_CONTEXT_OP_TYPE_FILL]);
end;

function TBLContextState.GetHints: TBLContextHints;
begin
  Result.FHandle := FHandle.hints;
end;

function TBLContextState.GetMetaMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.metaMatrix;
end;

function TBLContextState.GetSavedStateCount: Integer;
begin
  Result := FHandle.savedStateCount;
end;

function TBLContextState.GetStrokeOptions: TBLStrokeOptions;
begin
  Result.FHandle := FHandle.strokeOptions;
end;

function TBLContextState.GetStrokeStyle: TBLStyleType;
begin
  Result := TBLStyleType(FHandle.styleType[BL_CONTEXT_OP_TYPE_STROKE]);
end;

function TBLContextState.GetTargetImage: IBLImage;
begin
  if (FHandle.targetImage = nil) then
    Result := nil
  else
    Result := TBLImage.Create(FHandle.targetImage^, True);
end;

function TBLContextState.GetTargetSize: TBLSize;
begin
  Result := TBLSize(FHandle.targetSize);
end;

function TBLContextState.GetStrokeAlpha: Double;
begin
  Result := FHandle.styleAlpha[BL_CONTEXT_OP_TYPE_STROKE];
end;

function TBLContextState.GetUserMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.userMatrix;
end;

{ TBLContext }

procedure TBLContext.BlitImage(const ADst: TBLPointI; const ASrc: IBLImage);
begin
  _BLCheck(blContextBlitImageI(@FHandle, @ADst, ASrc.Handle, nil));
end;

procedure TBLContext.BlitImage(const ADst: TBLRect; const ASrc: IBLImage);
begin
  _BLCheck(blContextBlitScaledImageD(@FHandle, @ADst, ASrc.Handle, nil));
end;

procedure TBLContext.BlitImage(const ADst: TBLPointI; const ASrc: IBLImage;
  const ASrcArea: TBLRectI);
begin
  _BLCheck(blContextBlitImageI(@FHandle, @ADst, ASrc.Handle, @ASrcArea));
end;

procedure TBLContext.BlitImage(const ADst: TBLPoint; const ASrc: IBLImage;
  const ASrcArea: TBLRectI);
begin
  _BLCheck(blContextBlitImageD(@FHandle, @ADst, ASrc.Handle, @ASrcArea));
end;

procedure TBLContext.BlitImage(const ADst: TBLPoint; const ASrc: IBLImage);
begin
  _BLCheck(blContextBlitImageD(@FHandle, @ADst, ASrc.Handle, nil));
end;

procedure TBLContext.BlitImage(const ADst: TBLRectI; const ASrc: IBLImage);
begin
  _BLCheck(blContextBlitScaledImageI(@FHandle, @ADst, ASrc.Handle, nil));
end;

procedure TBLContext.BlitImage(const ADst: TBLRect; const ASrc: IBLImage;
  const ASrcArea: TBLRectI);
begin
  _BLCheck(blContextBlitScaledImageD(@FHandle, @ADst, ASrc.Handle, @ASrcArea));
end;

procedure TBLContext.AfterConstruction;
begin
  inherited;
  { Blend2D requires that floating-point calculations do not raise exceptions.}
  SetExceptionMask(exAllArithmeticExceptions);
end;

procedure TBLContext.BlitImage(const ADst: TBLRectI; const ASrc: IBLImage;
  const ASrcArea: TBLRectI);
begin
  _BLCheck(blContextBlitScaledImageI(@FHandle, @ADst, ASrc.Handle, @ASrcArea));
end;

procedure TBLContext.ClearAll;
begin
  _BLCheck(blContextClearAll(@FHandle));
end;

procedure TBLContext.ClearRect(const ARect: TBLRectI);
begin
  _BLCheck(blContextClearRectI(@FHandle, @ARect));
end;

procedure TBLContext.ClearRect(const ARect: TBLRect);
begin
  _BLCheck(blContextClearRectD(@FHandle, @ARect));
end;

procedure TBLContext.ClearRect(const AX, AY, AW, AH: Double);
var
  R: TBLRect;
begin
  R.Reset(AX, AY, AW, AH);
  _BLCheck(blContextClearRectD(@FHandle, @R));
end;

procedure TBLContext.ClipToRect(const AX, AY, AW, AH: Double);
var
  R: TBLRect;
begin
  R.Reset(AX, AY, AW, AH);
  _BLCheck(blContextClipToRectD(@FHandle, @R));
end;

procedure TBLContext.ClipToRect(const ARect: TBLRect);
begin
  _BLCheck(blContextClipToRectD(@FHandle, @ARect));
end;

procedure TBLContext.ClipToRect(const ARect: TBLRectI);
begin
  _BLCheck(blContextClipToRectI(@FHandle, @ARect));
end;

constructor TBLContext.Create;
begin
  inherited Create;
  blContextInit(@FHandle);
end;

constructor TBLContext.Create(const ATarget: IBLImage);
begin
  inherited Create;
  if (ATarget = nil) then
    blContextInit(@FHandle)
  else
    _BLCheck(blContextInitAs(@FHandle, ATarget.Handle, nil));
end;

constructor TBLContext.Create(const ATarget: IBLImage;
  const ACreateInfo: TBLContextCreateInfo);
begin
  inherited Create;
  if (ATarget = nil) then
    blContextInit(@FHandle)
  else
    _BLCheck(blContextInitAs(@FHandle, ATarget.Handle, @ACreateInfo.FHandle));
end;

destructor TBLContext.Destroy;
begin
  blContextDestroy(@FHandle);
  inherited;
end;

function TBLContext.Equals(Obj: TObject): Boolean;
begin
  if (Obj = nil) then
    Result := (Self = nil)
  else if (Obj = Self) then
    Result := True
  else if (Obj is TBLContext) then
    Result := (FHandle.impl = TBLContext(Obj).FHandle.impl)
  else
    Result := False;
end;

function TBLContext.Equals(const AOther: IBLContext): Boolean;
begin
  if (AOther = nil) then
    Result := (Self = nil)
  else
    Result := (FHandle.impl = AOther.Handle.impl)
end;

procedure TBLContext.FillAll;
begin
  _BLCheck(blContextFillAll(@FHandle));
end;

procedure TBLContext.FillBox(const AX0, AY0, AX1, AY1: Double);
var
  Box: TBLBox;
begin
  Box.Reset(AX0, AY0, AX1, AY1);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXD, @Box));
end;

procedure TBLContext.FillBox(const ABox: TBLBox);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXD, @ABox));
end;

procedure TBLContext.FillBox(const ABox: TBLBoxI);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXI, @ABox));
end;

procedure TBLContext.FillBoxArray(const ABoxes: PBLBox; const ACount: Integer);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View));
end;

procedure TBLContext.FillBoxArray(const ABoxes: TArray<TBLBoxI>);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(Pointer(ABoxes), Length(ABoxes));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View));
end;

procedure TBLContext.FillBoxArray(const ABoxes: TBLArrayView<TBLBoxI>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @ABoxes));
end;

procedure TBLContext.FillBoxArray(const ABoxes: PBLBoxI; const ACount: Integer);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View));
end;

procedure TBLContext.FillBoxArray(const ABoxes: TBLArrayView<TBLBox>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @ABoxes));
end;

procedure TBLContext.FillBoxArray(const ABoxes: TArray<TBLBox>);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(Pointer(ABoxes), Length(ABoxes));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View));
end;

procedure TBLContext.FillChord(const ACX, ACY, ARX, ARY, AStart,
  ASweep: Double);
var
  Chord: TBLArc;
begin
  Chord.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @Chord));
end;

procedure TBLContext.FillChord(const ACX, ACY, AR, AStart, ASweep: Double);
var
  Chord: TBLArc;
begin
  Chord.Reset(ACX, ACY, AR, AStart, ASweep);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @Chord));
end;

procedure TBLContext.FillChord(const AChord: TBLArc);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @AChord));
end;

procedure TBLContext.FillCircle(const ACX, ACY, AR: Double);
var
  Circle: TBLCircle;
begin
  Circle.Reset(ACX, ACY, AR);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @Circle));
end;

procedure TBLContext.FillCircle(const ACircle: TBLCircle);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @ACircle));
end;

procedure TBLContext.FillEllipse(const AEllipse: TBLEllipse);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @AEllipse));
end;

procedure TBLContext.FillEllipse(const ACX, ACY, ARX, ARY: Double);
var
  Ellipse: TBLEllipse;
begin
  Ellipse.Reset(ACX, ACY, ARX, ARY);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @Ellipse));
end;

procedure TBLContext.FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
  const AGlyphRun: TBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AFont = nil) then
    Font := nil
  else
    Font := AFont.Handle;

  _BLCheck(blContextFillGlyphRunD(@FHandle, @ADst, Font, @AGlyphRun));
end;

procedure TBLContext.FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
  const AGlyphRun: TBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AFont = nil) then
    Font := nil
  else
    Font := AFont.Handle;

  _BLCheck(blContextFillGlyphRunI(@FHandle, @ADst, Font, @AGlyphRun));
end;

procedure TBLContext.FillGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
  const AGlyphRun: PBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AGlyphRun <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillGlyphRunD(@FHandle, @ADst, Font, Pointer(AGlyphRun)));
  end;
end;

procedure TBLContext.FillGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
  const AGlyphRun: PBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AGlyphRun <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillGlyphRunI(@FHandle, @ADst, Font, Pointer(AGlyphRun)));
  end;
end;

procedure TBLContext.FillPath(const APath: IBLPath);
begin
  if (APath <> nil) then
    _BLCheck(blContextFillPathD(@FHandle, APath.Handle));
end;

procedure TBLContext.FillPie(const ACX, ACY, AR, AStart, ASweep: Double);
var
  Pie: TBLArc;
begin
  Pie.Reset(ACX, ACY, AR, AStart, ASweep);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @Pie));
end;

procedure TBLContext.FillPie(const ACX, ACY, ARX, ARY, AStart, ASweep: Double);
var
  Pie: TBLArc;
begin
  Pie.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @Pie));
end;

procedure TBLContext.FillPie(const APie: TBLArc);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @APie));
end;

procedure TBLContext.FillPolygon(const APoly: TBLArrayView<TBLPoint>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @APoly));
end;

procedure TBLContext.FillPolygon(const APoly: TArray<TBLPointI>);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View));
end;

procedure TBLContext.FillPolygon(const APoly: TBLArrayView<TBLPointI>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @APoly));
end;

procedure TBLContext.FillPolygon(const APoly: PBLPointI; const ACount: Integer);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View));
end;

procedure TBLContext.FillPolygon(const APoly: TArray<TBLPoint>);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View));
end;

procedure TBLContext.FillPolygon(const APoly: PBLPoint; const ACount: Integer);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View));
end;

procedure TBLContext.FillRect(const AX, AY, AW, AH: Double);
var
  R: TBLRect;
begin
  R.Reset(AX, AY, AW, AH);
  _BLCheck(blContextFillRectD(@FHandle, @R));
end;

procedure TBLContext.FillRect(const ARect: TBLRect);
begin
  _BLCheck(blContextFillRectD(@FHandle, @ARect));
end;

procedure TBLContext.FillRect(const ARect: TBLRectI);
begin
  _BLCheck(blContextFillRectI(@FHandle, @ARect));
end;

procedure TBLContext.FillRectArray(const ARects: PBLRect;
  const ACount: Integer);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View));
end;

procedure TBLContext.FillRectArray(const ARects: TArray<TBLRectI>);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(Pointer(ARects), Length(ARects));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View));
end;

procedure TBLContext.FillRectArray(const ARects: TBLArrayView<TBLRect>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @ARects));
end;

procedure TBLContext.FillRectArray(const ARects: TArray<TBLRect>);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(Pointer(ARects), Length(ARects));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View));
end;

procedure TBLContext.FillRectArray(const ARects: TBLArrayView<TBLRectI>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @ARects));
end;

procedure TBLContext.FillRectArray(const ARects: PBLRectI;
  const ACount: Integer);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View));
end;

procedure TBLContext.FillRegion(const ARegion: IBLRegion);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_REGION, ARegion.Handle));
end;

procedure TBLContext.FillRoundRect(const ARect: TBLRect; const AR: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(ARect, AR);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.FillRoundRect(const ARoundRect: TBLRoundRect);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @ARoundRect));
end;

procedure TBLContext.FillRoundRect(const AX, AY, AW, AH, AR: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(AX, AY, AW, AH, AR);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.FillRoundRect(const ARect: TBLRect; const ARX,
  ARY: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(ARect, ARX, ARY);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.FillRoundRect(const AX, AY, AW, AH, ARX, ARY: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(AX, AY, AW, AH, ARX, ARY);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.FillText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: UTF8String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF8));
  end;
end;

procedure TBLContext.FillText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF16));
  end;
end;

procedure TBLContext.FillText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF16));
  end;
end;

procedure TBLContext.FillText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: UCS4String);
var
  Font: PBLFontCore;
begin
  if (AText <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText) - 1, BL_TEXT_ENCODING_UTF32));
  end;
end;

procedure TBLContext.FillText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: UCS4String);
var
  Font: PBLFontCore;
begin
  if (AText <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText) - 1, BL_TEXT_ENCODING_UTF32));
  end;
end;

procedure TBLContext.FillText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: UTF8String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextFillTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF8));
  end;
end;

procedure TBLContext.FillTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double);
var
  Triangle: TBLTriangle;
begin
  Triangle.Reset(AX0, AY0, AX1, AY1, AX2, AY2);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @Triangle));
end;

procedure TBLContext.FillTriangle(const ATriangle: TBLTriangle);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @ATriangle));
end;

procedure TBLContext.Finish;
begin
  _BLCheck(blContextEnd(@FHandle));
end;

procedure TBLContext.Flush(const AFlags: TBLContextFlushFlags);
begin
  _BLCheck(blContextFlush(@FHandle, Cardinal(AFlags)));
end;

function TBLContext.GetApproximationOptions: TBLApproximationOptions;
begin
  Result.FHandle := FHandle.impl.state.approximationOptions;
end;

function TBLContext.GetCompOp: TBLCompOp;
begin
  Result := TBLCompOp(FHandle.impl.state.compOp);
end;

function TBLContext.GetContextType: TBLContextType;
begin
  Result := TBLContextType(FHandle.impl.contextType);
end;

function TBLContext.GetFillAlpha: Double;
begin
  Result := FHandle.impl.state.styleAlpha[BL_CONTEXT_OP_TYPE_FILL];
end;

function TBLContext.GetFillColor: TBLRgba32;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  Result.Reset(TBLRgba(Style.rgba));
end;

function TBLContext.GetFillColor64: TBLRgba64;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  Result.Reset(TBLRgba(Style.rgba));
end;

function TBLContext.GetFillColorF: TBLRgba;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  Result := TBLRgba(Style.rgba);
end;

function TBLContext.GetFillGradient: IBLGradient;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  Result := TBLGradient.Create(Style.gradient, True);
end;

function TBLContext.GetFillPattern: IBLPattern;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  Result := TBLPattern.Create(Style.pattern, True);
end;

function TBLContext.GetFillRule: TBLFillRule;
begin
  Result := TBLFillRule(FHandle.impl.state.fillRule);
end;

procedure TBLContext.GetFillStyle(out ARgba: TBLRgba32);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  ARgba.Reset(TBLRgba(Style.rgba));
end;

procedure TBLContext.GetFillStyle(out ARgba: TBLRgba64);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  ARgba.Reset(TBLRgba(Style.rgba));
end;

procedure TBLContext.GetFillStyle(out AGradient: IBLGradient);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  AGradient := TBLGradient.Create(Style.gradient, True);
end;

procedure TBLContext.GetFillStyle(out ARgba: TBLRgba);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  ARgba := TBLRgba(Style.rgba);
end;

procedure TBLContext.GetFillStyle(out APattern: IBLPattern);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetFillStyle(@FHandle, @Style));
  APattern := TBLPattern.Create(Style.pattern, True);
end;

function TBLContext.GetFillStyle: TBLStyleType;
begin
  Result := TBLStyleType(FHandle.impl.state.styleType[BL_CONTEXT_OP_TYPE_FILL]);
end;

function TBLContext.GetFlattenMode: TBLFlattenMode;
begin
  Result := TBLFlattenMode(FHandle.impl.state.approximationOptions.flattenMode);
end;

function TBLContext.GetFlattenTolerance: Double;
begin
  Result := FHandle.impl.state.approximationOptions.flattenTolerance;
end;

function TBLContext.GetGlobalAlpha: Double;
begin
  Result := FHandle.impl.state.globalAlpha;
end;

function TBLContext.GetGradientQuality: TBLGradientQuality;
begin
  Result := TBLGradientQuality(FHandle.impl.state.hints.gradientQuality);
end;

function TBLContext.GetHandle: PBLContextCore;
begin
  Result := @FHandle;
end;

function TBLContext.GetHints: TBLContextHints;
begin
  Result.FHandle := FHandle.impl.state.hints;
end;

function TBLContext.GetIsNone: Boolean;
begin
  Result := ((FHandle.impl.implTraits and BL_IMPL_TRAIT_NULL) <> 0);
end;

function TBLContext.GetMetaMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.impl.state.metaMatrix;
end;

function TBLContext.GetPatternQuality: TBLPatternQuality;
begin
  Result := TBLPatternQuality(FHandle.impl.state.hints.patternQuality);
end;

function TBLContext.GetRenderingQuality: TBLRenderingQuality;
begin
  Result := TBLRenderingQuality(FHandle.impl.state.hints.renderingQuality);
end;

function TBLContext.GetSavedStateCount: Integer;
begin
  Result := FHandle.impl.state.savedStateCount;
end;

function TBLContext.GetStrokeAlpha: Double;
begin
  Result := FHandle.impl.state.styleAlpha[BL_CONTEXT_OP_TYPE_STROKE];
end;

function TBLContext.GetStrokeColor: TBLRgba32;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  Result.Reset(TBLRgba(Style.rgba));
end;

function TBLContext.GetStrokeColor64: TBLRgba64;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  Result.Reset(TBLRgba(Style.rgba));
end;

function TBLContext.GetStrokeColorF: TBLRgba;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  Result := TBLRgba(Style.rgba);
end;

function TBLContext.GetStrokeDashArray: TArray<Double>;
begin
  Result := TBLUtils.BLArrayToArray<Double>(FHandle.impl.state.strokeOptions.dashArray);
end;

function TBLContext.GetStrokeDashOffset: Double;
begin
  Result := FHandle.impl.state.strokeOptions.dashOffset;
end;

function TBLContext.GetStrokeEndCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FHandle.impl.state.strokeOptions.options.endCap);
end;

function TBLContext.GetStrokeGradient: IBLGradient;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  Result := TBLGradient.Create(Style.gradient, True);
end;

function TBLContext.GetStrokeJoin: TBLStrokeJoin;
begin
  Result := TBLStrokeJoin(FHandle.impl.state.strokeOptions.options.join);
end;

function TBLContext.GetStrokeMiterLimit: Double;
begin
  Result := FHandle.impl.state.strokeOptions.miterLimit;
end;

function TBLContext.GetStrokeOptions: TBLStrokeOptions;
begin
  Result.FHandle := FHandle.impl.state.strokeOptions;
end;

function TBLContext.GetStrokePattern: IBLPattern;
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  Result := TBLPattern.Create(Style.pattern, True);
end;

function TBLContext.GetStrokeStartCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FHandle.impl.state.strokeOptions.options.startCap);
end;

procedure TBLContext.GetStrokeStyle(out ARgba: TBLRgba64);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  ARgba.Reset(TBLRgba(Style.rgba));
end;

procedure TBLContext.GetStrokeStyle(out ARgba: TBLRgba32);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  ARgba.Reset(TBLRgba(Style.rgba));
end;

procedure TBLContext.GetStrokeStyle(out AGradient: IBLGradient);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  AGradient := TBLGradient.Create(Style.gradient, True);
end;

procedure TBLContext.GetStrokeStyle(out ARgba: TBLRgba);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  ARgba := TBLRgba(Style.rgba);
end;

procedure TBLContext.GetStrokeStyle(out APattern: IBLPattern);
var
  Style: BLStyleCore;
begin
  _BLCheck(blContextGetStrokeStyle(@FHandle, @Style));
  APattern := TBLPattern.Create(Style.pattern, True);
end;

function TBLContext.GetStrokeStyle: TBLStyleType;
begin
  Result := TBLStyleType(FHandle.impl.state.styleType[BL_CONTEXT_OP_TYPE_STROKE]);
end;

function TBLContext.GetStrokeTransformOrder: TBLStrokeTransformOrder;
begin
  Result := TBLStrokeTransformOrder(FHandle.impl.state.strokeOptions.options.transformOrder);
end;

function TBLContext.GetStrokeWidth: Double;
begin
  Result := FHandle.impl.state.strokeOptions.width;
end;

function TBLContext.GetTargetHeight: Double;
begin
  Result := FHandle.impl.state.targetSize.h;
end;

function TBLContext.GetTargetImage: IBLImage;
begin
  if (FHandle.impl.state.targetImage = nil) then
    Result := nil
  else
    Result := TBLImage.Create(FHandle.impl.state.targetImage^, True);
end;

function TBLContext.GetTargetSize: TBLSize;
begin
  Result.FHandle := FHandle.impl.state.targetSize;
end;

function TBLContext.GetTargetWidth: Double;
begin
  Result := FHandle.impl.state.targetSize.w;
end;

function TBLContext.GetUserMatrix: TBLMatrix2D;
begin
  Result.FHandle := FHandle.impl.state.userMatrix;
end;

procedure TBLContext.PostRotate(const AAngle: Double; const APoint: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := APoint.FHandle.x;
  Data[2] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLContext.PostRotate(const AAngle: Double;
  const APoint: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := APoint.FHandle.x;
  Data[2] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLContext.PostRotate(const AAngle: Double);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE, @AAngle));
end;

procedure TBLContext.PostRotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_ROTATE_PT, @Data));
end;

procedure TBLContext.PostScale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLContext.PostScale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLContext.PostScale(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @APoint));
end;

procedure TBLContext.PostScale(const APoint: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := APoint.FHandle.x;
  Data[1] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SCALE, @Data));
end;

procedure TBLContext.PostSkew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @Data));
end;

procedure TBLContext.PostSkew(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_SKEW, @APoint));
end;

procedure TBLContext.PostTransform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSFORM, @AMatrix));
end;

procedure TBLContext.PostTranslate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLContext.PostTranslate(const APoint: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := APoint.FHandle.x;
  Data[1] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @Data));
end;

procedure TBLContext.PostTranslate(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_POST_TRANSLATE, @APoint));
end;

function TBLContext.QueryAccumulatedErrorFlags: TBLContextErrorFlags;
begin
  _BLCheck(blContextQueryProperty(@FHandle,
    BL_CONTEXT_PROPERTY_ACCUMULATED_ERROR_FLAGS, @Result));
end;

function TBLContext.QueryThreadCount: Integer;
begin
  _BLCheck(blContextQueryProperty(@FHandle, BL_CONTEXT_PROPERTY_THREAD_COUNT,
    @Result));
end;

procedure TBLContext.Reset;
begin
  _BLCheck(blContextReset(@FHandle));
end;

procedure TBLContext.ResetMatrix;
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_RESET, nil));
end;

procedure TBLContext.Restore(const ACookie: TBLContextCookie);
begin
  _BLCheck(blContextRestore(@FHandle, @ACookie));
end;

procedure TBLContext.Restore;
begin
  _BLCheck(blContextRestore(@FHandle, nil));
end;

procedure TBLContext.RestoreClipping;
begin
  _BLCheck(blContextRestoreClipping(@FHandle));
end;

procedure TBLContext.Rotate(const AAngle: Double; const APoint: TBLPoint);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := APoint.FHandle.x;
  Data[2] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLContext.Rotate(const AAngle, AX, AY: Double);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := AX;
  Data[2] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLContext.Rotate(const AAngle: Double);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE, @AAngle));
end;

procedure TBLContext.Rotate(const AAngle: Double; const APoint: TBLPointI);
var
  Data: array [0..2] of Double;
begin
  Data[0] := AAngle;
  Data[1] := APoint.FHandle.x;
  Data[2] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_ROTATE_PT, @Data));
end;

procedure TBLContext.Save(out ACookie: TBLContextCookie);
begin
  _BLCheck(blContextSave(@FHandle, @ACookie));
end;

procedure TBLContext.Save;
begin
  _BLCheck(blContextSave(@FHandle, nil));
end;

procedure TBLContext.Scale(const AXY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AXY;
  Data[1] := AXY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLContext.Scale(const APoint: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := APoint.FHandle.x;
  Data[1] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLContext.Scale(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @Data));
end;

procedure TBLContext.Scale(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SCALE, @APoint));
end;

procedure TBLContext.SetCompOp(const AValue: TBLCompOp);
begin
  _BLCheck(blContextSetCompOp(@FHandle, Ord(AValue)));
end;

procedure TBLContext.SetFillAlpha(const AValue: Double);
begin
  _BLCheck(blContextSetFillAlpha(@FHandle, AValue));
end;

procedure TBLContext.SetFillColor(const AValue: TBLRgba32);
begin
  _BLCheck(blContextSetFillStyleRgba32(@FHandle, AValue.FHandle.value));
end;

procedure TBLContext.SetFillColor64(const AValue: TBLRgba64);
begin
  _BLCheck(blContextSetFillStyleRgba64(@FHandle, AValue.FHandle.value));
end;

procedure TBLContext.SetFillColorF(const AValue: TBLRgba);
begin
  _BLCheck(blContextSetFillStyleRgba(@FHandle, @AValue));
end;

procedure TBLContext.SetFillGradient(const AValue: IBLGradient);
begin
  if (AValue <> nil) then
    _BLCheck(blContextSetFillStyleObject(@FHandle, AValue.Handle));
end;

procedure TBLContext.SetFillPattern(const AValue: IBLPattern);
begin
  if (AValue <> nil) then
    _BLCheck(blContextSetFillStyleObject(@FHandle, AValue.Handle));
end;

procedure TBLContext.SetFillRule(const AValue: TBLFillRule);
begin
  _BLCheck(blContextSetFillRule(@FHandle, Ord(AValue)));
end;

procedure TBLContext.SetFillStyle(const ARgba: TBLRgba64);
begin
  _BLCheck(blContextSetFillStyleRgba64(@FHandle, ARgba.FHandle.value));
end;

procedure TBLContext.SetFillStyle(const ARgba: TBLRgba32);
begin
  _BLCheck(blContextSetFillStyleRgba32(@FHandle, ARgba.FHandle.value));
end;

procedure TBLContext.SetFillStyle(const APattern: IBLPattern);
begin
  if (APattern <> nil) then
    _BLCheck(blContextSetFillStyleObject(@FHandle, APattern.Handle));
end;

procedure TBLContext.SetFillStyle(const AImage: IBLImage);
begin
  if (AImage <> nil) then
    _BLCheck(blContextSetFillStyleObject(@FHandle, AImage.Handle));
end;

procedure TBLContext.SetFillStyle(const ARgba: TBLRgba);
begin
  _BLCheck(blContextSetFillStyleRgba(@FHandle, @ARgba));
end;

procedure TBLContext.SetFillStyle(const AGradient: IBLGradient);
begin
  if (AGradient <> nil) then
    _BLCheck(blContextSetFillStyleObject(@FHandle, AGradient.Handle));
end;

procedure TBLContext.SetFlattenMode(const AValue: TBLFlattenMode);
begin
  _BLCheck(blContextSetFlattenMode(@FHandle, Ord(AValue)));
end;

procedure TBLContext.SetFlattenTolerance(const AValue: Double);
begin
  _BLCheck(blContextSetFlattenTolerance(@FHandle, AValue));
end;

procedure TBLContext.SetGlobalAlpha(const AValue: Double);
begin
  _BLCheck(blContextSetGlobalAlpha(@FHandle, AValue));
end;

procedure TBLContext.SetGradientQuality(const AValue: TBLGradientQuality);
begin
  _BLCheck(blContextSetHint(@FHandle, BL_CONTEXT_HINT_GRADIENT_QUALITY, Ord(AValue)));
end;

procedure TBLContext.SetHints(const AValue: TBLContextHints);
begin
  _BLCheck(blContextSetHints(@FHandle, @AValue.FHandle));
end;

procedure TBLContext.SetPatternQuality(const AValue: TBLPatternQuality);
begin
  _BLCheck(blContextSetHint(@FHandle, BL_CONTEXT_HINT_PATTERN_QUALITY, Ord(AValue)));
end;

procedure TBLContext.SetRenderingQuality(const AValue: TBLRenderingQuality);
begin
  _BLCheck(blContextSetHint(@FHandle, BL_CONTEXT_HINT_RENDERING_QUALITY, Ord(AValue)));
end;

procedure TBLContext.SetStrokeAlpha(const AValue: Double);
begin
  _BLCheck(blContextSetStrokeAlpha(@FHandle, AValue));
end;

procedure TBLContext.SetStrokeColor(const AValue: TBLRgba32);
begin
  _BLCheck(blContextSetStrokeStyleRgba32(@FHandle, AValue.FHandle.value));
end;

procedure TBLContext.SetStrokeColor64(const AValue: TBLRgba64);
begin
  _BLCheck(blContextSetStrokeStyleRgba64(@FHandle, AValue.FHandle.value));
end;

procedure TBLContext.SetStrokeColorF(const AValue: TBLRgba);
begin
  _BLCheck(blContextSetStrokeStyleRgba(@FHandle, @AValue));
end;

procedure TBLContext.SetStrokeDashArray(const AValue: TArray<Double>);
var
  Value: IBLArray;
begin
  Value := TBLUtils.ArrayToBLArray<Double>(AValue);
  _BLCheck(blContextSetStrokeDashArray(@FHandle, Value.Handle));
end;

procedure TBLContext.SetStrokeDashOffset(const AValue: Double);
begin
  _BLCheck(blContextSetStrokeDashOffset(@FHandle, AValue));
end;

procedure TBLContext.SetStrokeEndCap(const AValue: TBLStrokeCap);
begin
  _BLCheck(blContextSetStrokeCap(@FHandle, BL_STROKE_CAP_POSITION_END, Ord(AValue)));
end;

procedure TBLContext.SetStrokeGradient(const AValue: IBLGradient);
begin
  if (AValue <> nil) then
    _BLCheck(blContextSetStrokeStyleObject(@FHandle, AValue.Handle));
end;

procedure TBLContext.SetStrokeJoin(const AValue: TBLStrokeJoin);
begin
  _BLCheck(blContextSetStrokeJoin(@FHandle, Ord(AValue)));
end;

procedure TBLContext.SetStrokeMiterLimit(const AValue: Double);
begin
  _BLCheck(blContextSetStrokeMiterLimit(@FHandle, AValue));
end;

procedure TBLContext.SetStrokeOptions(const AValue: TBLStrokeOptions);
begin
  _BLCheck(blContextSetStrokeOptions(@FHandle, @AValue.FHandle));
end;

procedure TBLContext.SetStrokePattern(const AValue: IBLPattern);
begin
  if (AValue <> nil) then
    _BLCheck(blContextSetStrokeStyleObject(@FHandle, AValue.Handle));
end;

procedure TBLContext.SetStrokeStartCap(const AValue: TBLStrokeCap);
begin
  _BLCheck(blContextSetStrokeCap(@FHandle, BL_STROKE_CAP_POSITION_START, Ord(AValue)));
end;

procedure TBLContext.SetStrokeStyle(const ARgba: TBLRgba64);
begin
  _BLCheck(blContextSetStrokeStyleRgba64(@FHandle, ARgba.FHandle.value));
end;

procedure TBLContext.SetStrokeStyle(const ARgba: TBLRgba32);
begin
  _BLCheck(blContextSetStrokeStyleRgba32(@FHandle, ARgba.FHandle.value));
end;

procedure TBLContext.SetStrokeStyle(const APattern: IBLPattern);
begin
  if (APattern <> nil) then
    _BLCheck(blContextSetStrokeStyleObject(@FHandle, APattern.Handle));
end;

procedure TBLContext.SetStrokeStyle(const AGradient: IBLGradient);
begin
  if (AGradient <> nil) then
    _BLCheck(blContextSetStrokeStyleObject(@FHandle, AGradient.Handle));
end;

procedure TBLContext.SetStrokeStyle(const AImage: IBLImage);
begin
  if (AImage <> nil) then
    _BLCheck(blContextSetStrokeStyleObject(@FHandle, AImage.Handle));
end;

procedure TBLContext.SetStrokeStyle(const ARgba: TBLRgba);
begin
  _BLCheck(blContextSetStrokeStyleRgba(@FHandle, @ARgba));
end;

procedure TBLContext.SetStrokeTransformOrder(
  const AValue: TBLStrokeTransformOrder);
begin
  _BLCheck(blContextSetStrokeTransformOrder(@FHandle, Ord(AValue)));
end;

procedure TBLContext.SetStrokeWidth(const AValue: Double);
begin
  _BLCheck(blContextSetStrokeWidth(@FHandle, AValue));
end;

procedure TBLContext.SetUserMatrix(const AValue: TBLMatrix2D);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_ASSIGN, @AValue));
end;

procedure TBLContext.Skew(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @APoint));
end;

procedure TBLContext.Skew(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_SKEW, @Data));
end;

procedure TBLContext.Start(const AImage: IBLImage;
  const ACreateInfo: TBLContextCreateInfo);
begin
  if (AImage <> nil) then
    _BLCheck(blContextBegin(@FHandle, AImage.Handle, @ACreateInfo.FHandle));
end;

procedure TBLContext.Start(const AImage: IBLImage);
begin
  if (AImage <> nil) then
    _BLCheck(blContextBegin(@FHandle, AImage.Handle, nil));
end;

procedure TBLContext.StrokeArc(const AArc: TBLArc);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARC, @AArc));
end;

procedure TBLContext.StrokeArc(const ACX, ACY, AR, AStart, ASweep: Double);
var
  Arc: TBLArc;
begin
  Arc.Reset(ACX, ACY, AR, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARC, @Arc));
end;

procedure TBLContext.StrokeArc(const ACX, ACY, ARX, ARY, AStart,
  ASweep: Double);
var
  Arc: TBLArc;
begin
  Arc.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARC, @Arc));
end;

procedure TBLContext.StrokeBox(const AX0, AY0, AX1, AY1: Double);
var
  Box: TBLBox;
begin
  Box.Reset(AX0, AY0, AX1, AY1);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXD, @Box));
end;

procedure TBLContext.StrokeBox(const ABox: TBLBox);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXD, @ABox));
end;

procedure TBLContext.StrokeBox(const ABox: TBLBoxI);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_BOXI, @ABox));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: PBLBoxI;
  const ACount: Integer);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: PBLBox;
  const ACount: Integer);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(ABoxes, ACount);
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: TBLArrayView<TBLBox>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @ABoxes));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: TBLArrayView<TBLBoxI>);
begin
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @ABoxes));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: TArray<TBLBoxI>);
var
  View: TBLArrayView<TBLBoxI>;
begin
  View.Reset(Pointer(ABoxes), Length(ABoxes));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXI, @View));
end;

procedure TBLContext.StrokeBoxArray(const ABoxes: TArray<TBLBox>);
var
  View: TBLArrayView<TBLBox>;
begin
  View.Reset(Pointer(ABoxes), Length(ABoxes));
  _BLCheck(blContextFillGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_BOXD, @View));
end;

procedure TBLContext.StrokeChord(const AChord: TBLArc);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @AChord));
end;

procedure TBLContext.StrokeChord(const ACX, ACY, AR, AStart, ASweep: Double);
var
  Chord: TBLArc;
begin
  Chord.Reset(ACX, ACY, AR, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @Chord));
end;

procedure TBLContext.StrokeChord(const ACX, ACY, ARX, ARY, AStart,
  ASweep: Double);
var
  Chord: TBLArc;
begin
  Chord.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_CHORD, @Chord));
end;

procedure TBLContext.StrokeCircle(const ACX, ACY, AR: Double);
var
  Circle: TBLCircle;
begin
  Circle.Reset(ACX, ACY, AR);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @Circle));
end;

procedure TBLContext.StrokeCircle(const ACircle: TBLCircle);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_CIRCLE, @ACircle));
end;

procedure TBLContext.StrokeEllipse(const AEllipse: TBLEllipse);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @AEllipse));
end;

procedure TBLContext.StrokeEllipse(const ACX, ACY, ARX, ARY: Double);
var
  Ellipse: TBLEllipse;
begin
  Ellipse.Reset(ACX, ACY, ARX, ARY);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ELLIPSE, @Ellipse));
end;

procedure TBLContext.StrokeGlyphRun(const ADst: TBLPointI; const AFont: IBLFont;
  const AGlyphRun: TBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AFont = nil) then
    Font := nil
  else
    Font := AFont.Handle;

  _BLCheck(blContextStrokeGlyphRunI(@FHandle, @ADst, Font, @AGlyphRun));
end;

procedure TBLContext.StrokeGlyphRun(const ADst: TBLPoint; const AFont: IBLFont;
  const AGlyphRun: TBLGlyphRun);
var
  Font: PBLFontCore;
begin
  if (AFont = nil) then
    Font := nil
  else
    Font := AFont.Handle;

  _BLCheck(blContextStrokeGlyphRunD(@FHandle, @ADst, Font, @AGlyphRun));
end;

procedure TBLContext.StrokeLine(const AX0, AY0, AX1, AY1: Double);
var
  Line: TBLLine;
begin
  Line.Reset(AX0, AY0, AX1, AY1);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_LINE, @Line));
end;

procedure TBLContext.StrokeLine(const AP0, AP1: TBLPoint);
var
  Line: TBLLine;
begin
  Line.Reset(AP0, AP1);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_LINE, @Line));
end;

procedure TBLContext.StrokeLine(const ALine: TBLLine);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_LINE, @ALine));
end;

procedure TBLContext.StrokePath(const APath: IBLPath);
begin
  if (APath <> nil) then
    _BLCheck(blContextStrokePathD(@FHandle, APath.Handle));
end;

procedure TBLContext.StrokePie(const ACX, ACY, ARX, ARY, AStart,
  ASweep: Double);
var
  Pie: TBLArc;
begin
  Pie.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @Pie));
end;

procedure TBLContext.StrokePie(const ACX, ACY, AR, AStart, ASweep: Double);
var
  Pie: TBLArc;
begin
  Pie.Reset(ACX, ACY, AR, AStart, ASweep);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @Pie));
end;

procedure TBLContext.StrokePie(const APie: TBLArc);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_PIE, @APie));
end;

procedure TBLContext.StrokePolygon(const APoly: TBLArrayView<TBLPoint>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @APoly));
end;

procedure TBLContext.StrokePolygon(const APoly: TArray<TBLPointI>);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View));
end;

procedure TBLContext.StrokePolygon(const APoly: PBLPoint;
  const ACount: Integer);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View));
end;

procedure TBLContext.StrokePolygon(const APoly: TArray<TBLPoint>);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGOND, @View));
end;

procedure TBLContext.StrokePolygon(const APoly: PBLPointI;
  const ACount: Integer);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @View));
end;

procedure TBLContext.StrokePolygon(const APoly: TBLArrayView<TBLPointI>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYGONI, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: TBLArrayView<TBLPointI>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: TArray<TBLPoint>);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: TBLArrayView<TBLPoint>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: PBLPoint;
  const ACount: Integer);
var
  View: TBLArrayView<TBLPoint>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINED, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: TArray<TBLPointI>);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(Pointer(APoly), Length(APoly));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @APoly));
end;

procedure TBLContext.StrokePolyline(const APoly: PBLPointI;
  const ACount: Integer);
var
  View: TBLArrayView<TBLPointI>;
begin
  View.Reset(APoly, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_POLYLINEI, @APoly));
end;

procedure TBLContext.StrokeRect(const ARect: TBLRectI);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_RECTI, @ARect));
end;

procedure TBLContext.StrokeRect(const ARect: TBLRect);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_RECTD, @ARect));
end;

procedure TBLContext.StrokeRect(const AX, AY, AW, AH: Double);
var
  Rect: TBLRect;
begin
  Rect.Reset(AX, AY, AW, AH);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_RECTD, @Rect));
end;

procedure TBLContext.StrokeRectArray(const ARects: PBLRect;
  const ACount: Integer);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View));
end;

procedure TBLContext.StrokeRectArray(const ARects: TArray<TBLRect>);
var
  View: TBLArrayView<TBLRect>;
begin
  View.Reset(Pointer(ARects), Length(ARects));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @View));
end;

procedure TBLContext.StrokeRectArray(const ARects: TBLArrayView<TBLRect>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTD, @ARects));
end;

procedure TBLContext.StrokeRectArray(const ARects: TBLArrayView<TBLRectI>);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @ARects));
end;

procedure TBLContext.StrokeRectArray(const ARects: PBLRectI;
  const ACount: Integer);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(ARects, ACount);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View));
end;

procedure TBLContext.StrokeRectArray(const ARects: TArray<TBLRectI>);
var
  View: TBLArrayView<TBLRectI>;
begin
  View.Reset(Pointer(ARects), Length(ARects));
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ARRAY_VIEW_RECTI, @View));
end;

procedure TBLContext.StrokeRoundRect(const AX, AY, AW, AH, AR: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(AX, AY, AW, AH, AR);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.StrokeRoundRect(const ARect: TBLRect; const AR: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(ARect, AR);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.StrokeRoundRect(const ARoundRect: TBLRoundRect);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @ARoundRect));
end;

procedure TBLContext.StrokeRoundRect(const ARect: TBLRect; const ARX,
  ARY: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(ARect, ARX, ARY);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.StrokeRoundRect(const AX, AY, AW, AH, ARX, ARY: Double);
var
  RoundRect: TBLRoundRect;
begin
  RoundRect.Reset(AX, AY, AW, AH, ARX, ARY);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_ROUND_RECT, @RoundRect));
end;

procedure TBLContext.StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: UCS4String);
var
  Font: PBLFontCore;
begin
  if (AText <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText) - 1, BL_TEXT_ENCODING_UTF32));
  end;
end;

procedure TBLContext.StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: UTF8String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF8));
  end;
end;

procedure TBLContext.StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: UCS4String);
var
  Font: PBLFontCore;
begin
  if (AText <> nil) then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText) - 1, BL_TEXT_ENCODING_UTF32));
  end;
end;

procedure TBLContext.StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: UTF8String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF8));
  end;
end;

procedure TBLContext.StrokeText(const ADst: TBLPoint; const AFont: IBLFont;
  const AText: String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextD(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF16));
  end;
end;

procedure TBLContext.StrokeText(const ADst: TBLPointI; const AFont: IBLFont;
  const AText: String);
var
  Font: PBLFontCore;
begin
  if (AText <> '') then
  begin
    if (AFont = nil) then
      Font := nil
    else
      Font := AFont.Handle;

    _BLCheck(blContextStrokeTextI(@FHandle, @ADst, Font, Pointer(AText), Length(AText), BL_TEXT_ENCODING_UTF16));
  end;
end;

procedure TBLContext.StrokeTriangle(const ATriangle: TBLTriangle);
begin
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @ATriangle));
end;

procedure TBLContext.StrokeTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double);
var
  Triangle: TBLTriangle;
begin
  Triangle.Reset(AX0, AY0, AX1, AY1, AX2, AY2);
  _BLCheck(blContextStrokeGeometry(@FHandle, BL_GEOMETRY_TYPE_TRIANGLE, @Triangle));
end;

procedure TBLContext.Transform(const AMatrix: TBLMatrix2D);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSFORM, @AMatrix));
end;

procedure TBLContext.Translate(const APoint: TBLPointI);
var
  Data: array [0..1] of Double;
begin
  Data[0] := APoint.FHandle.x;
  Data[1] := APoint.FHandle.y;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

procedure TBLContext.Translate(const AX, AY: Double);
var
  Data: array [0..1] of Double;
begin
  Data[0] := AX;
  Data[1] := AY;
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @Data));
end;

procedure TBLContext.Translate(const APoint: TBLPoint);
begin
  _BLCheck(blContextMatrixOp(@FHandle, BL_MATRIX2D_OP_TRANSLATE, @APoint));
end;

procedure TBLContext.UserToMeta;
begin
  _BLCheck(blContextUserToMeta(@FHandle));
end;

{$ENDREGION 'Context'}

{$REGION 'Runtime'}

{ TBLRuntimeBuildInfo }

function TBLRuntimeBuildInfo.GetBaselineCpuFeatures: TBLRuntimeCpuFeatures;
begin
  Byte(Result) := FHandle.baselineCpuFeatures;
end;

function TBLRuntimeBuildInfo.GetBuildType: TBLRuntimeBuildType;
begin
  Result := TBLRuntimeBuildType(FHandle.buildType);
end;

function TBLRuntimeBuildInfo.GetSupportedCpuFeatures: TBLRuntimeCpuFeatures;
begin
  Byte(Result) := FHandle.supportedCpuFeatures;
end;

{ TBLRuntimeSystemInfo }

function TBLRuntimeSystemInfo.GetCpuArch: TBLRuntimeCpuArch;
begin
  Result := TBLRuntimeCpuArch(FHandle.cpuArch);
end;

function TBLRuntimeSystemInfo.GetCpuFeatures: TBLRuntimeCpuFeatures;
begin
  Byte(Result) := FHandle.cpuFeatures;
end;

{ TBLRuntime }

class procedure TBLRuntime.Cleanup(const AFlags: TBLRuntimeCleanupFlags);
begin
  _BLCheck(blRuntimeCleanup(Byte(AFlags)));
end;

class procedure TBLRuntime.LogMessage(const AMessage: String;
  const AArgs: array of const);
begin
  _BLCheck(blRuntimeMessageOut(MarshaledAString(UTF8String(Format(AMessage, AArgs)))));
end;

class procedure TBLRuntime.LogMessage(const AMessage: String);
begin
  _BLCheck(blRuntimeMessageOut(MarshaledAString(UTF8String(AMessage))));
end;

class procedure TBLRuntime.QueryBuildInfo(out AInfo: TBLRuntimeBuildInfo);
begin
  _BLCheck(blRuntimeQueryInfo(BL_RUNTIME_INFO_TYPE_BUILD, @AInfo));
end;

class procedure TBLRuntime.QueryResourceInfo(out AInfo: TBLRuntimeResourceInfo);
begin
  _BLCheck(blRuntimeQueryInfo(BL_RUNTIME_INFO_TYPE_RESOURCE, @AInfo));
end;

class procedure TBLRuntime.QuerySystemInfo(out AInfo: TBLRuntimeSystemInfo);
begin
  _BLCheck(blRuntimeQueryInfo(BL_RUNTIME_INFO_TYPE_SYSTEM, @AInfo));
end;

{$ENDREGION 'Runtime'}

{$REGION 'Internal'}

procedure _BLCheck(const AResult: BLResultCode);
begin
  if (AResult <> BL_SUCCESS) and Assigned(GErrorHandler) then
    GErrorHandler(TBLResultCode(AResult), GErrorUserData);
end;

{$ENDREGION 'Internal'}

{$REGION 'Initialization'}
initialization
  BLSetExceptionErrorHandler;

{$ENDREGION 'Initialization'}

end.
