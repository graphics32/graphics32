unit GR32.CPUID;

{***** BEGIN LICENSE BLOCK *****
 Version: MPL 1.1

 The contents of this file are subject to the Mozilla Public License Version 1.1
 (the "License"); you may not use this file except in compliance with the
 License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS IS" basis,
 WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 the specific language governing rights and limitations under the License.

 The Original Code is the FastCode CPUID code.

 The Initial Developer of the Original Code is
 Roelof Engelbrecht <roelof@cox-internet.com>. Portions created by
 the Initial Developer are Copyright (C) 2004 by the Initial Developer.
 All Rights Reserved.

 Contributor(s): Dennis Passmore <Dennis_Passmore@ ultimatesoftware.com>,
                 Dennis Kjaer Christensen <marianndkc@home3.gvdnet.dk>,
                 Jouni Turunen <jouni.turunen@NOSPAM.iki.fi>.

***** END LICENSE BLOCK *****

Version  Changes
-------  ------
 3.0.2   27 Apr 2006 : AMD X2 text changed from 'AMD_64_SSE3' to 'AMD_64X2'
 3.0.1   18 Apr 2006 : Bug in Yohan fctPMY target fixed, was incorrectly set to fctPMD
 3.0.0   27 Feb 2006 : Added new 2006 computed targets. Added Yonah and Presler
                       Removed Prescott, Banias, AMD XP                    (JT)

This is a merge of several different forks of the FastcodeCPUID unit.
Common to them, compared to version 3.0.2 of the original unit, is added support
for 64-bit and various CPU features.
Code and changes related to "Fastcode targets" has been removed.

 Contributor(s):

 - Johan Bontes <johan@NOSPAM.digitsolutions.nl>
   23 Mar 2016 : Added x64 code and new CPUID features

 - John O'Harrow <john@elmcrest.demon.co.uk>
    1-Aug-2012 : Added VerifyOSSupportForYMMRegisters by Philipp S
   3-juli-2012 : Added Win64 support - GetCPUID by Philipp S, IsCPUID_Available advice from Remy Lebeau,
                 see http://www.intel.com/content/www/us/en/processors/processor-identification-cpuid-instruction-note.html
   20-Nov-2007 : Added SSE4A, SSE4.1, SSE4.2 and SSE5 detections.
   20-Nov-2006 : Added SSSE3 Detection

}

interface

{$I GR32.inc}

{$IFDEF PUREPASCAL}
{$message warn 'GR32.CPUID should not be compiled in PUREPASCAL mode}
{$ENDIF}

{$if (defined(CompilerVersion)) and (CompilerVersion >= 17.0)} // Delphi 2005
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
{$ifend}

type
  TCPUVendor = (
    cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel,
    cvTransmeta, cvNexGen, cvRise, cvUMC, cvNSC, cvSiS, cvAMDEarly,
    cvTransMeta2, cvVIA, cvVortex, cvVM_KVM, cvVM_Microsoft,
    cvVM_Parallels, cvVM_VMWare, cvVM_XEN
  );

  TCPUInstructionSet = (
    isFPU,      {80x87}
    isTSC,      {RDTSC}
    isCX8,      {CMPXCHG8B}
    isSEP,      {SYSENTER/SYSEXIT}
    isCMOV,     {CMOVcc, and if isFPU, FCMOVcc/FCOMI}
    isMMX,      {MMX}
    isFXSR,     {FXSAVE/FXRSTOR}
    isSSE,      {SSE}
    isSSE2,     {SSE2}
    isSSE3,     {SSE3*}
    isMONITOR,  {MONITOR/MWAIT*}
    isCX16,     {CMPXCHG16B*}
    isX64,      {AMD AMD64* or Intel EM64T*}
    isExMMX,    {MMX+ - AMD only}
    isEx3DNow,  {3DNow!+ - AMD only}
    is3DNow,    {3DNow! - AMD only}
    isSSSE3,    {Supplemental SSE3}
    isSSE41,    {SSE 4.1}
    isSSE42,    {SSE 4.2}
    isAES,      {AES support}
    isAVX,      {AVX}
    isPopCnt,   {Popcnt, lzcnt, tzcnt}
    isXSAVE,    {XSAVE}
    isRDTSCP,   {Read synchronized RDTSCP}
    isTBM,      {Trailing bit manipulations}
    isFMA4,     {4 operand FMA instructions support}
    isXOP,      {XOP support}
    isSSE4A,    {SSE 4a support, note that popcount has its own flag}
    isABM,      {Advanced bit manipulation}
    isLAHF,     {Lahf, Sahf support in 64-bit}
    isPCLMULQDQ,{PCLMULQDQ support}
    isFMA,      {Fused multiply and add}
    isMOVBE,    {move Big Endian}
    isF16C,     {half precision FP support}
    isRDRAND,   {Onchip random generator}
    isBMI1,     {Bit manipulation instruction set 1}
    isAVX2,     {Advanced vector instructions 2}
    isBMI2,     {Bit manipulation instruction set 2}
    isERMS,     {Enhanced REP MOVSB/STOSB}
    isINVPCID,  {INVPCID instructions}
    isRTM,      {Transactional Synchronization Extensions}
    isMPX,      {Memory Protection Extensions}
    isAVX512f,  {AVX-512 Foundation}
    isAVX512dq, {AVX-512 Doubleword and Quadword Instructions}
    isRDSEED,   {RDSEED instruction}
    isADX,      {Multi-Precision Add-Carry Instruction Extensions}
    isPCOMMIT,  {PCOMMIT instruction}
    isCLFLUSHOPT,{CLFLUSHOPT instruction}
    isCLWB,     {CLWB instruction}
    isAVX512pf, {AVX-512 Prefetch Instructions}
    isAVX512er, {AVX-512 Exponential and Reciprocal Instructions}
    isAVX512cd, {AVX-512 Conflict Detection Instructions}
    isSHA,      {SHA extensions}
    isAVX512bw, {AVX-512 Byte and Word Instructions}
    isAVX512vl, {AVX-512 Vector Length Extensions}
    isPREFETCHWT1,{PREFETCHWT1 instruction}
    isAVX512vbmi {AVX-512 Vector Bit Manipulation Instructions}
  );
  TInstructionSupport = set of TCPUInstructionSet;

  TCPU = record
  private
    procedure GetCPUVendor;
    procedure GetCPUFeatures;
    procedure GetCPUExtendedFeatures;
    procedure GetCPUExtendedFeatures7;
    procedure GetProcessorCacheInfo;
    procedure GetExtendedProcessorCacheInfo;
    procedure VerifyOSSupportForXMMRegisters;
    function IsXmmYmmOSEnabled: boolean;
    procedure VerifyOSSupportForYMMRegisters;
    function GetVendorName: string; overload;
  public
    Vendor: TCPUVendor;
    Signature: Cardinal;
    EffFamily: Byte;    // ExtendedFamily + Family
    EffModel: Byte;     // (ExtendedModel shl 4) + Model
    EffModelBasic: Byte;// Just Model (not ExtendedModel shl 4) + Model)
    CodeL1CacheSize,    // Kilobytes, or micro-ops for Pentium 4
    DataL1CacheSize,    // Kilobytes
    L2CacheSize,        // Kilobytes
    L3CacheSize: Word;  // Kilobytes
    InstructionSupport: TInstructionSupport;

    property VendorName: string read GetVendorName;

    class function GetCPUInfo: TCPU; static;
    class function GetVendorName(Vendor: TCPUVendor): string; overload; static;
    class function GetInstructionSetName(InstructionSet: TCPUInstructionSet): string; static;
  end;

implementation

const
  sVendorNames: array[TCPUVendor] of string = (
    'Unknown', 'AMD', 'Centaur (VIA)', 'Cyrix', 'Intel', 'Transmeta',
    'NexGen', 'Rise', 'UMC', 'National Semiconductor', 'SiS','AMD K5 engineering sample',
    'TransMeta', 'VIA','Vortex', 'KVM_VM', 'Microsoft_VM',
    'Parallels_VM', 'VMWare_VM', 'XEN_VM'
  );

  sInstructionSetNames: array[TCPUInstructionSet] of string = (
    'FPU', 'TSC', 'CX8', 'SEP', 'CMOV', 'MMX', 'FXSR', 'SSE', 'SSE2', 'SSE3',
    'MONITOR', 'CX16', 'X64', 'MMX+', '3DNow!+', '3DNow!','SSSE3','SSE4.1',
    'SSE4.2','AES','AVX','PopCnt','XSAVE','RDTSCP','TBM','FMA4','XOP','SSE4A',
    'ABM','LAHF','PCLMULQDQ','FMA','MOVBE','F16C','RDRAND','BMI1','AVX2','BMI2',
    'ERMS','INVPCID','RTM','MPX', 'AVX512f','AVX512dq','RDSEED','ADX','PCOMMIT',
    'CLFLUSHOPT','CLWB','AVX512pf','AVX512er','AVX512cd','SHA','AVX512bw','AVX512vl',
    'PREFETCHWT1','AVX512vbmi'
  );

type
  TRegisters = record
    EAX,
      EBX,
      ECX,
      EDX: Cardinal;
  end;

  TVendorStr = string[12];

  TCpuFeatures = (
    {in EDX}
    cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
    cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
    cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
    cfFXSR, cfSSE, cfSSE2, cfSS, cfHyperThreading, cfTM, cfIA_64, cfPBE,
    {in ECX}
    cfSSE3, cfPCLMULQDQ, cf_c2, cfMON, cfDS_CPL, cf_c5, cf_c6, cfEIST,
    cfTM2, cfSSSE3, cfCID, cf_c11, cfFMA, cfCX16, cfxTPR, cf_c15,
    cf_c16, cf_c17, cf_c18, cfSSE41, cfSSE42, cf_c21, cfMovBE, cfPOPCNT,
    cf_c24, cfAES, cfXSAVE, cfOSXSAVE, cfAVX, cfF16C, cfRDRAND, cfRAZ
  );
  TCpuFeatureSet = set of TCpuFeatures;

  TCpuExtendedFeatures = (
    {in EDX}
    cefFPU, cefVME, cefDE, cefPSE, cefTSC, cefMSR, cefPAE, cefMCE,
    cefCX8, cefAPIC, cef_10, cefSEP, cefMTRR, cefPGE, cefMCA, cefCMOV,
    cefPAT, cefPSE36, cef_18, ceMPC, ceNX, cef_21, cefExMMX, cefMMX,
    cefFXSR, cefFFXSR, cefPage1GB, cefRDTSCP, cef_28, cefLM, cefEx3DNow, cef3DNow,
    {in ECX}
    cefLahfSahf, cefCmpLegacy, cefSVM, cefExtApicSpace, cefAltMovCR8, cefABM, cefSSE4A, cefMisAlignSSE,
    cef3DNOWPrefetch, cefOSVW, cefIBS, cefXOP, cefSKINIT, cefWDT, cef_c14, cefLWP,
    cefFMA4, cefTCE, cef_c18, cefNodeId, cef_c20, cefTBM, cefTopologyExtensions, cefPERFCTR_core,
    cefPERFCTR_nb, cef_c25, cefDBX, cefPERFTSC, cefPCX_l2i, cef_c29, cef_c30, cef_c31
  );
  TCpuExtendedFeatureSet = set of TCpuExtendedFeatures;

  TCPUExtendedFeatures7 = ( {EAX = 7, ECX = 0}
    {in EBX}
    ce7FSGSbase, ce7TSC_Adjust, ce7SGX, ce7BMI1, ce7HLE, ce7AVX2, ce7_06, ce7SMEP,
    ce7BMI2, ce7ERMS, ce7INVPCID, ce7RTM, ce7PQM, ce7NoFPUcsds, ce7MPX, ce7PQE,
    ce7AVX512f, ce7AVX512dq, ce7RDSEED, ce7ADX, ce7SMAP, ce7AVX512ifma, ce7PCOMMIT, ce7CLFLUSHOPT,
    ce7CLWB, ce7ProcessorTrace, ce7AVX512pf, ce7AVX512er, ce7AVX512cd, ce7SHA, ce7AVX512bw, ce7AVX512vl,
    {in ECX}
    ce7cPREFETCHWT1, ce7cAVX512vbmi, ce7c_02, ce7c_03, ce7c_04, ce7c_05, ce7c_06, ce7c_07,
    ce7c_08, ce7c_09, ce7c_10, ce7c_11, ce7c_12, ce7c_13, ce7c_14, ce7c_15,
    ce7c_16, ce7c_17, ce7c_18, ce7c_19, ce7c_20, ce7c_21, ce7c_22, ce7c_23,
    ce7c_24, ce7c_25, ce7c_26, ce7c_27, ce7c_28, ce7c_29, ce7c_30, ce7c_31
  );
  TCpuExtendedFeature7Set = set of TCpuExtendedFeatures7;


const
  VendorIDString: array[TCPUVendor] of TVendorStr = (
    '',
    'AuthenticAMD', 'CentaurHauls', 'CyrixInstead', 'GenuineIntel',
    'GenuineTMx86', 'NexGenDriven', 'RiseRiseRise', 'UMC UMC UMC ',
    'Geode by NSC', 'SiS SiS SiS ', 'AMDisbetter!', 'TransmetaCPU',
    'VIA VIA VIA ', 'Vortex86 SoC', 'KVMKVMKVM   ', 'Microsoft Hv',
    '  lrpepyh vr', 'VMwareVMware', 'XenVMMXenVMM'
  );

  {CPU signatures}
  IntelLowestSEPSupportSignature = $633;
  K7DuronA0Signature    = $630;
  C3Samuel2EffModel     = 7;
  C3EzraEffModel        = 8;
  // For a list of Intel CPU models by family, microarchitecture and core, see: https://en.wikichip.org/wiki/intel/cpuid

function IsCPUID_Available: Boolean; register;
asm
{$IFDEF CPUx86}
  PUSHFD                 {save EFLAGS to stack}
  POP     EAX            {store EFLAGS in EAX}
  MOV     EDX, EAX       {save in EDX for later testing}
  XOR     EAX, $200000;  {flip ID bit in EFLAGS}
  PUSH    EAX            {save new EFLAGS value on stack}
  POPFD                  {replace current EFLAGS value}
  PUSHFD                 {get new EFLAGS}
  POP     EAX            {store new EFLAGS in EAX}
  XOR     EAX, EDX       {check if ID bit changed}
  JZ      @exit          {no, CPUID not available}
  MOV     EAX, True      {yes, CPUID is available}
@exit:
{$ELSE}
  MOV     EAX, True      {x64 always has CPUID}
{$ENDIF}
end;

function IsFPU_Available: Boolean;
{$IFDEF CPUx86}
var
  _FCW, _FSW: Word;
asm
  MOV     EAX, False     {initialize return register}
  MOV     _FSW, $5A5A    {store a non-zero value}
  FNINIT                 {must use non-wait form}
  FNSTSW  _FSW           {store the status}
  CMP     _FSW, 0        {was the correct status read?}
  JNE     @exit          {no, FPU not available}
  FNSTCW  _FCW           {yes, now save control word}
  MOV     DX, _FCW       {get the control word}
  AND     DX, $103F      {mask the proper status bits}
  CMP     DX, $3F        {is a numeric processor installed?}
  JNE     @exit          {no, FPU not installed}
  MOV     EAX, True      {yes, FPU is installed}
@exit:
{$ELSE}
asm
  MOV     EAX, True      {Every X64 has an FPU}
{$ENDIF}
end;

procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
asm
{$ifdef CPUx86}
  PUSH    EBX                         {save affected registers}
  PUSH    EDI
  MOV     EDI, Registers
  XOR     EBX, EBX                    {clear EBX register}
  XOR     ECX, ECX                    {clear ECX register}
  XOR     EDX, EDX                    {clear EDX register}
  DB $0F, $A2                         {CPUID opcode}
  MOV     TRegisters(EDI).&EAX, EAX   {save EAX register}
  MOV     TRegisters(EDI).&EBX, EBX   {save EBX register}
  MOV     TRegisters(EDI).&ECX, ECX   {save ECX register}
  MOV     TRegisters(EDI).&EDX, EDX   {save EDX register}
  POP     EDI                         {restore registers}
  POP     EBX
{$else X64}
  PUSH    RBX
  PUSH    RDI
  MOV     RDI, Registers
  MOV     EAX, ECX
  XOR     EBX, EBX
  XOR     ECX, ECX
  XOR     EDX, EDX
  CPUID
  MOV     TRegisters(RDI).&EAX, EAX
  MOV     TRegisters(RDI).&EBX, EBX
  MOV     TRegisters(RDI).&ECX, ECX
  MOV     TRegisters(RDI).&EDX, EDX
  POP     RDI
  POP     RBX
{$endif}
end;

procedure TCPU.GetCPUVendor;
var
  VendorStr: TVendorStr;
  Registers: TRegisters;
begin
  {call CPUID function 0}
  GetCPUID(0, Registers);

  {get vendor string}
  SetLength(VendorStr, 12);
  Move(Registers.EBX, VendorStr[1], 4);
  Move(Registers.EDX, VendorStr[5], 4);
  Move(Registers.ECX, VendorStr[9], 4);

  {get CPU vendor from vendor string}
  Vendor:= High(TCPUVendor);
  while (VendorStr <> VendorIDString[Vendor]) and (Vendor > Low(TCPUVendor)) do
    Dec(Vendor);
end;

procedure TCPU.GetCPUFeatures;
{ preconditions:
  1. maximum CPUID must be at least $00000001
  2. GetCPUVendor must have been called }
type
  _Int64 = packed record
    Lo: Longword;
    Hi: Longword;
  end;
var
  Registers: TRegisters;
  CpuFeatures: TCpuFeatureSet;
begin
  {call CPUID function $00000001}
  GetCPUID($00000001, Registers);

  {get CPU signature}
  Signature:= Registers.EAX;

  {extract effective processor family and model}
  EffFamily:= Signature and $00000F00 shr 8;
  EffModel:= Signature and $000000F0 shr 4;
  EffModelBasic:= EffModel;
  if EffFamily = $F then
  begin
    EffFamily:= EffFamily + (Signature and $0FF00000 shr 20);
    EffModel:= EffModel + (Signature and $000F0000 shr 12);
  end;

  {get CPU features}
  Move(Registers.EDX, _Int64(CpuFeatures).Lo, 4);
  Move(Registers.ECX, _Int64(CpuFeatures).Hi, 4);

  {get instruction support}
  if cfFPU in CpuFeatures then Include(InstructionSupport, isFPU);
  if cfTSC in CpuFeatures then Include(InstructionSupport, isTSC);
  if cfCX8 in CpuFeatures then Include(InstructionSupport, isCX8);
  if cfSEP in CpuFeatures then
  begin
    Include(InstructionSupport, isSEP);
    {for Intel CPUs, qualify the processor family and model to ensure that the
     SYSENTER/SYSEXIT instructions are actually present - see Intel Application
     Note AP-485}
    if (Vendor = cvIntel) and (Signature and $0FFF3FFF < IntelLowestSEPSupportSignature) then
      Exclude(InstructionSupport, isSEP);
  end;
  if cfCMOV   in CpuFeatures then Include(InstructionSupport, isCMOV);
  if cfFXSR   in CpuFeatures then Include(InstructionSupport, isFXSR);
  if cfMMX    in CpuFeatures then Include(InstructionSupport, isMMX);
  if cfSSE    in CpuFeatures then Include(InstructionSupport, isSSE);
  if cfSSE2   in CpuFeatures then Include(InstructionSupport, isSSE2);
  if cfSSE3   in CpuFeatures then Include(InstructionSupport, isSSE3);
  if cfSSSE3  in CpuFeatures then Include(InstructionSupport, isSSSE3);
  if cfSSE41  in CpuFeatures then Include(InstructionSupport, isSSE41);
  if cfSSE42  in CpuFeatures then Include(InstructionSupport, isSSE42);
  if cfAES    in CpuFeatures then Include(InstructionSupport, isAES);
  if cfAVX    in CpuFeatures then Include(InstructionSupport, isAVX);
  if cfPOPCNT in CpuFeatures then Include(InstructionSupport, isPopCnt);
  if cfCX16   in CpuFeatures then Include(InstructionSupport, isCX16);
  if cfXSAVE  in CpuFeatures then Include(InstructionSupport, isXSAVE);
  if cfPCLMULQDQ in CpuFeatures then Include(InstructionSupport, isPCLMULQDQ);
  if cfFMA    in CpuFeatures then Include(InstructionSupport, isFMA);
  if cfMovBE  in CpuFeatures then Include(InstructionSupport, isMovBE);
  if cfF16C   in CpuFeatures then Include(InstructionSupport, isF16C);
  if cfRDRAND in CpuFeatures then Include(InstructionSupport, isRDRAND);

  if {(Vendor = cvIntel) and }(cfMON in CpuFeatures) then Include(InstructionSupport, isMONITOR);
end;

procedure TCPU.GetCPUExtendedFeatures;
{preconditions: maximum extended CPUID >= $80000001}
type
  _Int64 = packed record
    Lo: Longword;
    Hi: Longword;
  end;
var
  Registers: TRegisters;
  CpuExFeatures: TCpuExtendedFeatureSet;
begin
  {call CPUID function $80000001}
  GetCPUID($80000001, Registers);

  {get CPU extended features}
  // Note: The various versions of FastcodeCPUID disagreed on the EDX/ECX order here
  Move(Registers.EDX, _Int64(CpuExFeatures).Lo, 4);
  Move(Registers.ECX, _Int64(CpuExFeatures).Hi, 4);

  {get instruction support}
  if cefLM        in CpuExFeatures then Include(InstructionSupport, isX64);
  if cefExMMX     in CpuExFeatures then Include(InstructionSupport, isExMMX);
  if cefEx3DNow   in CpuExFeatures then Include(InstructionSupport, isEx3DNow);
  if cef3DNow     in CpuExFeatures then Include(InstructionSupport, is3DNow);
  if cefRDTSCP    in CpuExFeatures then Include(InstructionSupport, isRDTSCP);
  if cefTBM       in CpuExFeatures then Include(InstructionSupport, isTBM);
  if cefFMA4      in CpuExFeatures then Include(InstructionSupport, isFMA4);
  if cefXOP       in CpuExFeatures then Include(InstructionSupport, isXOP);
  if cefSSE4A     in CpuExFeatures then Include(InstructionSupport, isSSE4A);
  if cefABM       in CpuExFeatures then Include(InstructionSupport, isABM);
  if cefLahfSahf  in CpuExFeatures then Include(InstructionSupport, isLAHF);
end;

procedure TCPU.GetCPUExtendedFeatures7;
type
  _Int64 = packed record
    Lo: Longword;
    Hi: Longword;
  end;
var
  Registers: TRegisters;
  Cpu7ExFeatures: TCpuExtendedFeature7Set;
begin
  {call CPUID function $80000001}
  GetCPUID($7, Registers);

  {get CPU extended features}
  Move(Registers.EBX, _Int64(Cpu7ExFeatures).Lo, 4);
  Move(Registers.ECX, _Int64(Cpu7ExFeatures).Hi, 4);

  {get instruction support}
  if ce7BMI1         in Cpu7ExFeatures then Include(InstructionSupport, isBMI1);
  if ce7AVX2         in Cpu7ExFeatures then Include(InstructionSupport, isAVX2);
  if ce7BMI2         in Cpu7ExFeatures then Include(InstructionSupport, isBMI2);
  if ce7ERMS         in Cpu7ExFeatures then Include(InstructionSupport, isERMS);
  if ce7INVPCID      in Cpu7ExFeatures then Include(InstructionSupport, isINVPCID);
  if ce7RTM          in Cpu7ExFeatures then Include(InstructionSupport, isRTM);
  if ce7MPX          in Cpu7ExFeatures then Include(InstructionSupport, isMPX);
  if ce7AVX512f      in Cpu7ExFeatures then Include(InstructionSupport, isAVX512f);
  if ce7AVX512dq     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512dq);
  if ce7RDSEED       in Cpu7ExFeatures then Include(InstructionSupport, isRDSEED);
  if ce7ADX          in Cpu7ExFeatures then Include(InstructionSupport, isADX);
  if ce7PCOMMIT      in Cpu7ExFeatures then Include(InstructionSupport, isPCOMMIT);
  if ce7CLFLUSHOPT   in Cpu7ExFeatures then Include(InstructionSupport, isCLFLUSHOPT);
  if ce7CLWB         in Cpu7ExFeatures then Include(InstructionSupport, isCLWB);
  if ce7AVX512pf     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512pf);
  if ce7AVX512er     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512er);
  if ce7AVX512cd     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512cd);
  if ce7SHA          in Cpu7ExFeatures then Include(InstructionSupport, isSHA);
  if ce7AVX512bw     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512bw);
  if ce7AVX512vl     in Cpu7ExFeatures then Include(InstructionSupport, isAVX512vl);
  if ce7cPREFETCHWT1 in Cpu7ExFeatures then Include(InstructionSupport, isPREFETCHWT1);
  if ce7cAVX512vbmi  in Cpu7ExFeatures then Include(InstructionSupport, isAVX512vbmi);
end;

procedure TCPU.GetProcessorCacheInfo;
{preconditions: 1. maximum CPUID must be at least $00000002
                2. GetCPUVendor must have been called}
type
  TConfigDescriptor = packed array[0..15] of Byte;
var
  Registers: TRegisters;
  i, j: Integer;
  QueryCount: Byte;
begin
  {call CPUID function 2}
  GetCPUID($00000002, Registers);
  QueryCount := Registers.EAX and $FF;
  for i := 1 to QueryCount do
  begin
    for j := 1 to 15 do
      {decode configuration descriptor byte}
      case TConfigDescriptor(Registers)[j] of
        $06: CodeL1CacheSize := 8;
        $08: CodeL1CacheSize := 16;
        $09: CodeL1CacheSize := 32;
        $0A: DataL1CacheSize := 8;
        $0C: DataL1CacheSize := 16;
        $0D: DataL1CacheSize := 16;
        $21: L2CacheSize := 256;
        $22: L3CacheSize := 512;
        $23: L3CacheSize := 1024;
        $25: L3CacheSize := 2048;
        $29: L3CacheSize := 4096;
        $2C: DataL1CacheSize := 32;
        $30: CodeL1CacheSize := 32;
        $39: L2CacheSize := 128;
        $3B: L2CacheSize := 128;
        $3C: L2CacheSize := 256;
        $3D: L2CacheSize := 384;
        $3E: L2CacheSize := 512;
        $40: {no 2nd-level cache or, if processor contains a valid 2nd-level
              cache, no 3rd-level cache}
          if L2CacheSize <> 0 then
            L3CacheSize := 0;
        $41: L2CacheSize := 128;
        $42: L2CacheSize := 256;
        $43: L2CacheSize := 512;
        $44: L2CacheSize := 1024;
        $45: L2CacheSize := 2048;
        $46: L3CacheSize := 4096;
        $47: L3CacheSize := 8192;
        $48: L2CacheSize := 3072;
        $49: if (Vendor = cvIntel) and (EffFamily = $F) and (EffModel = 6) then
               L3CacheSize := 4096
             else
               L2CacheSize := 4096;
        $4A: L3CacheSize := 6144;
        $4B: L3CacheSize := 8192;
        $4C: L3CacheSize := 12288;
        $4D: L3CacheSize := 16384;
        $4E: L2CacheSize := 6144;
        $60: DataL1CacheSize := 16;
        $66: DataL1CacheSize := 8;
        $67: DataL1CacheSize := 16;
        $68: DataL1CacheSize := 32;
        $70: if not (Vendor in [cvCyrix, cvNSC]) then
               CodeL1CacheSize := 12; {K micro-ops}
        $71: CodeL1CacheSize := 16; {K micro-ops}
        $72: CodeL1CacheSize := 32; {K micro-ops}
        $78: L2CacheSize := 1024;
        $79: L2CacheSize := 128;
        $7A: L2CacheSize := 256;
        $7B: L2CacheSize := 512;
        $7C: L2CacheSize := 1024;
        $7D: L2CacheSize := 2048;
        $7F: L2CacheSize := 512;
        $80: if Vendor in [cvCyrix, cvNSC] then
          begin {Cyrix and NSC only - 16 KB unified L1 cache}
            CodeL1CacheSize := 8;
            DataL1CacheSize := 8;
          end;
        $82: L2CacheSize := 256;
        $83: L2CacheSize := 512;
        $84: L2CacheSize := 1024;
        $85: L2CacheSize := 2048;
        $86: L2CacheSize := 512;
        $87: L2CacheSize := 1024;
        $D0: L3CacheSize := 512;
        $D1: L3CacheSize := 1024;
        $D2: L3CacheSize := 2048;
        $D6: L3CacheSize := 1024;
        $D7: L3CacheSize := 2048;
        $D8: L3CacheSize := 4096;
        $DC: L3CacheSize := 1536;
        $DD: L3CacheSize := 3072;
        $DE: L3CacheSize := 6144;
        $E2: L3CacheSize := 2048;
        $E3: L3CacheSize := 4096;
        $E4: L3CacheSize := 8192;
        $EA: L3CacheSize := 12288;
        $EB: L3CacheSize := 18432;
        $EC: L3CacheSize := 24576;
      end;
    if i < QueryCount then
      GetCPUID(2, Registers);
  end;
end;

procedure TCPU.GetExtendedProcessorCacheInfo;
{preconditions: 1. maximum extended CPUID must be at least $80000006
                2. GetCPUVendor and GetCPUFeatures must have been called}
var
  Registers: TRegisters;
begin
  {call CPUID function $80000005}
  GetCPUID($80000005, Registers);

  {get L1 cache size}
  {Note: Intel does not support function $80000005 for L1 cache size, so ignore.
         Cyrix returns CPUID function 2 descriptors (already done), so ignore.}
  if not (Vendor in [cvIntel, cvCyrix]) then
  begin
    CodeL1CacheSize := Registers.EDX shr 24;
    DataL1CacheSize := Registers.ECX shr 24;
  end;

  {call CPUID function $80000006}
  GetCPUID($80000006, Registers);

  {get L2 cache size}
  if (Vendor = cvAMD) and (Signature and $FFF = K7DuronA0Signature) then
    {workaround for AMD Duron Rev A0 L2 cache size erratum - see AMD Technical
     Note TN-13}
    L2CacheSize := 64
  else
  if (Vendor = cvCentaur) and (EffFamily = 6) and (EffModel in [C3Samuel2EffModel, C3EzraEffModel]) then
    {handle VIA (Centaur) C3 Samuel 2 and Ezra non-standard encoding}
    L2CacheSize := Registers.ECX shr 24
  else {standard encoding}
    L2CacheSize := Registers.ECX shr 16;
end;

procedure TCPU.VerifyOSSupportForXMMRegisters;
begin
{$ifdef CPUx86}
  {try a SSE instruction that operates on XMM registers}
  try
    asm
      DB $0F, $54, $C0  // ANDPS XMM0, XMM0
    end
  except
    {if it fails, assume that none of the SSE instruction sets are available}
    Exclude(InstructionSupport, isSSE);
    Exclude(InstructionSupport, isSSE2);
    Exclude(InstructionSupport, isSSE3);
    Exclude(InstructionSupport, isSSSE3);
    Exclude(InstructionSupport, isSSE41);
    Exclude(InstructionSupport, isSSE42);
    Exclude(InstructionSupport, isSSE4A);
  end;
{$ELSE}
  {do nothing}
{$ENDIF}
end;

function TCPU.IsXmmYmmOSEnabled: boolean;
asm
{$IFDEF CPUx86}
  push ebx
{$ELSE CPUx64}
  mov r10, rbx
{$ENDIF}
  mov   eax, 1
  cpuid
  bt    ecx, 27         // CPUID.1:ECX.OSXSAVE[bit 27] = 1 means that XGETBV is enabled for application use; this also implies XGETBV is an available instruction
  jnc @not_supported

  xor   ecx, ecx        //Specify control register XCR0 = XFEATURE_ENABLED_MASK register
  db 0Fh, 01h, 0D0h     // XGETBV //Reads XCR (extended control register) -> EDX:EAX

  // NB: LGDT eax = db 0Fh, 01h = privileged instruction, so don't go here unless XGETBV is allowed/enabled
  // CHECK XFEATURE_ENABLED_MASK[2:1] = 11b
  and   eax, 06h        // 06h= 00000000000000000000000000000110b
  cmp   eax, 06h        // check OS has enabled both XMM (bit 1) and YMM (bit 2) state management support
  jne @not_supported

  mov   eax, 1          // Result := True
  jmp @out

@not_supported:
  xor   eax, eax        // Result := False

@out:
{$IFDEF CPUx86}
  pop   ebx
{$ELSE CPUx64}
  mov   rbx, r10
{$ENDIF}
end;

// http://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions/
// Necessary to check that IsXmmYmmOSEnabled = true before using AVX, AVX2, FMA, etc. instructions!
procedure TCPU.VerifyOSSupportForYMMRegisters;
begin
  if not IsXmmYmmOSEnabled then
    Exclude(InstructionSupport, isAVX);
end;

function TCPU.GetVendorName: string;
begin
  Result := sVendorNames[Vendor];
end;

class function TCPU.GetVendorName(Vendor: TCPUVendor): string;
begin
  Result := sVendorNames[Vendor];
end;

class function TCPU.GetInstructionSetName(InstructionSet: TCPUInstructionSet): string;
begin
  Result := sInstructionSetNames[InstructionSet];
end;

class function TCPU.GetCPUInfo: TCPU;
var
  Registers: TRegisters;
  MaxCPUID: Cardinal;
  MaxExCPUID: Cardinal;
begin
  {initialize - just to be sure}
  Result := Default(TCPU);

  try
    if not IsCPUID_Available then
    begin
      if IsFPU_Available then
        Include(Result.InstructionSupport, isFPU);
    end else
    begin
      {get maximum CPUID input value}
      GetCPUID($00000000, Registers);
      MaxCPUID:= Registers.EAX;

      {get CPU vendor - Max CPUID will always be >= 0}
      Result.GetCPUVendor;

      {get CPU features if available}
      if MaxCPUID >= $00000001 then
        Result.GetCPUFeatures;

      {get cache info if available}
      if MaxCPUID >= $00000002 then
        Result.GetProcessorCacheInfo;

      if MaxCPUID >= $00000007 then
        Result.GetCPUExtendedFeatures7;

      {get maximum extended CPUID input value}
      GetCPUID($80000000, Registers);
      MaxExCPUID:= Registers.EAX;

      {get CPU extended features if available}
      if MaxExCPUID >= $80000001 then
        Result.GetCPUExtendedFeatures;

      {verify operating system support for XMM registers}
      if isSSE in Result.InstructionSupport then
        Result.VerifyOSSupportForXMMRegisters;

      { verify operating system support for YMM registers }
      if isAVX in Result.InstructionSupport then
        Result.VerifyOSSupportForYMMRegisters;

      {get extended cache features if available}
      {Note: ignore processors that only report L1 cache info,
       i.e. have a MaxExCPUID = $80000005}
      if MaxExCPUID >= $80000006 then
        Result.GetExtendedProcessorCacheInfo;
    end;
  except
    {silent exception - should not occur, just ignore}
  end;
end;

end.
