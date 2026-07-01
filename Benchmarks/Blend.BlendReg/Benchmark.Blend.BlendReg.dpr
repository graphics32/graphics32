program Benchmark.Blend.BlendReg;

{$APPTYPE CONSOLE}

{$I GR32.inc}

uses
  SysUtils,
  Spring.Benchmark,
  GR32,
  GR32_Bindings,
  GR32_Blend,
  GR32.Blend.Pascal,
  GR32.Blend.Assembler,
  GR32.Blend.SSE2,
  GR32.Types.SIMD;

//------------------------------------------------------------------------------
// BlendReg_SSE2_Org
//
// Baseline; Copy of BlendReg_SSE2 from June 2026
//------------------------------------------------------------------------------
function BlendReg_SSE2_Org(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb

{$IFDEF TARGET_x86}
        MOVD      XMM0,EAX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        MOV       ECX,bias_ptr       // ECX   <-  Pointer to Bias
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 Fa 00 Fa
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM0  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[ECX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
{$ENDIF}

{$IFDEF TARGET_x64}
        MOVD      XMM0,ECX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
{$IFNDEF FPC}
        MOV       RAX,bias_ptr       // RAX   <-  Pointer to Bias
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 ** 00 **
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM2  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[RAX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE2_Org
//
// BlendReg_SSE2_Org modified with early exits on fully transparent or fully
// opaque foreground.
//------------------------------------------------------------------------------
function BlendReg_SSE2_BoundaryFixes(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb

{$IFDEF TARGET_x86}
        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        MOV       ECX,bias_ptr       // ECX   <-  Pointer to Bias
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 Fa 00 Fa
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM0  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[ECX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
        RET
@1:     MOV       EAX,EDX
@2:
{$ENDIF}

{$IFDEF TARGET_x64}
        MOV       EAX,ECX
        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,ECX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
{$IFNDEF FPC}
        MOV       RAX,bias_ptr       // RAX   <-  Pointer to Bias
{$ELSE}
        MOV       RAX,[RIP+bias_ptr] // XXX : Enabling PIC by relative offsetting for x64
{$ENDIF}
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 ** 00 **
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM2  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **
        PADDW     XMM2,[RAX]         // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
        RET
@1:     MOV       EAX,EDX
@2:
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE2_No_bias_ptr
//
// BlendReg_SSE2_BoundaryFixes modified with register-based bias generation to
// eliminate bias_ptr memory lookup.
//------------------------------------------------------------------------------
function BlendReg_SSE2_No_bias_ptr(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Fargb - Bargb) + Bargb

{$IFDEF TARGET_x86}
        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 Fa 00 Fa
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM0  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **

        PCMPEQW   XMM1,XMM1
        PSRLW     XMM1,15
        PSLLW     XMM1,7             // XMM1  <-  00 00 00 00 00 80 00 80 00 80 00 80

        PADDW     XMM2,XMM1          // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
        RET
@1:     MOV       EAX,EDX
@2:
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST      ECX,$FF000000
        JZ        @1
        CMP       ECX,$FF000000
        JNC       @2

        MOVD      XMM0,ECX           // XMM0  <-  00 00 00 00 00 00 00 00 00 00 00 00 Fa Fr Fg Fb
        PXOR      XMM3,XMM3          // XMM3  <-  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        MOVD      XMM2,EDX           // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Ba Br Bg Bb
        PUNPCKLBW XMM0,XMM3          // XMM0  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PUNPCKLBW XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb
        MOVQ      XMM1,XMM0          // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fr 00 Fg 00 Fb
        PSHUFLW   XMM1,XMM1,$FF      // XMM1  <-  00 00 00 00 00 00 00 00 00 Fa 00 Fa 00 Fa 00 Fa
        PSUBW     XMM0,XMM2          // XMM0  <-  00 00 00 00 00 00 00 00 00 Da 00 Dr 00 Dg 00 Db
        PSLLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 Ba 00 Br 00 Bg 00 Bb 00
        PMULLW    XMM0,XMM1          // XMM0  <-  00 00 00 00 00 00 00 00 Pa ** Pr ** Pg ** Pb **

        PCMPEQW   XMM1,XMM1
        PSRLW     XMM1,15
        PSLLW     XMM1,7             // XMM1  <-  00 00 00 00 00 80 00 80 00 80 00 80

        PADDW     XMM2,XMM1          // add bias
        PADDW     XMM2,XMM0          // XMM2  <-  00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb **
        PSRLW     XMM2,8             // XMM2  <-  00 00 00 00 00 00 00 00 00 Qa ** Qr ** Qg ** Qb
        PACKUSWB  XMM2,XMM3          // XMM2  <-  00 00 00 00 00 00 00 00 00 00 00 00 Qa Qr Qg Qb
        MOVD      EAX,XMM2           // EAX   <-  Za Zr Zg Zb
        OR        EAX,$FF000000      // EAX   <-  FF Zr Zg Zb
        RET
@1:     MOV       EAX,EDX
        RET
@2:     MOV       EAX,ECX
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE41_No_bias_ptr
//
// BlendReg_SSE2_No_bias_ptr modified to use the SSE4.1 PMOVZXBW instruction to
// unpack the color components.
//------------------------------------------------------------------------------
function BlendReg_SSE41_No_bias_ptr(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$IFDEF TARGET_x86}
        TEST      EAX,$FF000000
        JZ        @1
        CMP       EAX,$FF000000
        JNC       @2

        MOVD      XMM0,EAX
        MOVD      XMM2,EDX
        PMOVZXBW  XMM0,XMM0
        PMOVZXBW  XMM2,XMM2

        PSHUFLW   XMM1,XMM0,$FF      // Broadcast Fa

        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1

        PCMPEQW   XMM1,XMM1
        PSRLW     XMM1,15
        PSLLW     XMM1,7             // XMM1 = 0080

        PADDW     XMM2,XMM1
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8

        PXOR      XMM3,XMM3
        PACKUSWB  XMM2,XMM3
        MOVD      EAX,XMM2
        OR        EAX,$FF000000
        RET
@1:     MOV       EAX,EDX
        RET
@2:     RET
{$ENDIF}

{$IFDEF TARGET_x64}
        TEST      ECX,$FF000000
        JZ        @1
        CMP       ECX,$FF000000
        JNC       @2

        MOVD      XMM0,ECX
        MOVD      XMM2,EDX
        PMOVZXBW  XMM0,XMM0
        PMOVZXBW  XMM2,XMM2

        PSHUFLW   XMM1,XMM0,$FF

        PSUBW     XMM0,XMM2
        PSLLW     XMM2,8
        PMULLW    XMM0,XMM1

        PCMPEQW   XMM1,XMM1
        PSRLW     XMM1,15
        PSLLW     XMM1,7

        PADDW     XMM2,XMM1
        PADDW     XMM2,XMM0
        PSRLW     XMM2,8

        PXOR      XMM3,XMM3
        PACKUSWB  XMM2,XMM3
        MOVD      EAX,XMM2
        OR        EAX,$FF000000
        RET
@1:     MOV       EAX,EDX
        RET
@2:     MOV       EAX,ECX
        RET
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE2_8081
//
// New BlendReg using the $8081 formula for lossless div255 calculation.
//------------------------------------------------------------------------------
function BlendReg_SSE2_8081(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) onto a background color (B),
  // using alpha channel value of F
  // Result Z = (Fa * Fargb + (255 - Fa) * Bargb + 128) / 255
  // Note: the background is assumed to be fully opaque (Ba=255).
  // Bit-perfect formula: Round(x / 255) = ((x * $8081 + $003FFF7F) shr 23)

{$IFDEF TARGET_x86}
        MOVD      XMM0, EAX           // XMM0 = [Fa, Fr, Fg, Fb]
        MOVD      XMM1, EDX           // XMM1 = [Ba, Br, Bg, Bb]
        PXOR      XMM5, XMM5
        PUNPCKLBW XMM0, XMM5          // words: [Fa, Fr, Fg, Fb]
        PUNPCKLBW XMM1, XMM5          // words: [Ba, Br, Bg, Bb]

        PSHUFLW   XMM2, XMM0, $FF     // words: [Fa, Fa, Fa, Fa]

        MOV       EAX, $00FF
        MOVD      XMM3, EAX
        PSHUFLW   XMM3, XMM3, 0       // words: [255, 255, 255, 255]
        PSUBW     XMM3, XMM2          // words: [255-Fa, ...]

        PMULLW    XMM0, XMM2          // Fa * F
        PMULLW    XMM1, XMM3          // (255-Fa) * B
        PADDW     XMM0, XMM1          // x = Fa*F + (255-Fa)*B

        // Multiplier trick x * $8081 using 32-bit math
        MOVDQA    XMM1, XMM0
        PUNPCKLWD XMM0, XMM5          // dwords: [x1, x0]
        PUNPCKHWD XMM1, XMM5          // dwords: [x3, x2]

        // x * $8081 = (x << 15) + (x << 7) + x
        MOVDQA    XMM2, XMM0
        PSLLD     XMM2, 15
        MOVDQA    XMM3, XMM0
        PSLLD     XMM3, 7
        PADDD     XMM0, XMM2
        PADDD     XMM0, XMM3

        MOVDQA    XMM2, XMM1
        PSLLD     XMM2, 15
        MOVDQA    XMM3, XMM1
        PSLLD     XMM3, 7
        PADDD     XMM1, XMM2
        PADDD     XMM1, XMM3

        MOV       EAX, $003FFF7F      // Bias
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PADDD     XMM0, XMM2
        PADDD     XMM1, XMM2

        PSRLD     XMM0, 23
        PSRLD     XMM1, 23

        PACKSSDW  XMM0, XMM1
        PACKUSWB  XMM0, XMM5
        MOVD      EAX, XMM0
        OR        EAX, $FF000000      // Force opaque result
        RET
{$ENDIF}

{$IFDEF TARGET_x64}
        MOVD      XMM0, ECX           // XMM0 = [Fa, Fr, Fg, Fb]
        MOVD      XMM1, EDX           // XMM1 = [Ba, Br, Bg, Bb]
        PXOR      XMM5, XMM5
        PUNPCKLBW XMM0, XMM5
        PUNPCKLBW XMM1, XMM5

        PSHUFLW   XMM2, XMM0, $FF     // Fa

        MOV       EAX, $00FF
        MOVD      XMM3, EAX
        PSHUFLW   XMM3, XMM3, 0       // 255
        PSUBW     XMM3, XMM2          // 255 - Fa

        PMULLW    XMM0, XMM2          // Fa * F
        PMULLW    XMM1, XMM3          // (255 - Fa) * B
        PADDW     XMM0, XMM1          // x

        MOVDQA    XMM1, XMM0
        PUNPCKLWD XMM0, XMM5          // [x1, x0]
        PUNPCKHWD XMM1, XMM5          // [x3, x2]

        MOVDQA    XMM2, XMM0
        PSLLD     XMM2, 15
        MOVDQA    XMM3, XMM0
        PSLLD     XMM3, 7
        PADDD     XMM0, XMM2
        PADDD     XMM0, XMM3

        MOVDQA    XMM2, XMM1
        PSLLD     XMM2, 15
        MOVDQA    XMM3, XMM1
        PSLLD     XMM3, 7
        PADDD     XMM1, XMM2
        PADDD     XMM1, XMM3

        MOV       EAX, $003FFF7F      // Bias
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PADDD     XMM0, XMM2
        PADDD     XMM1, XMM2

        PSRLD     XMM0, 23
        PSRLD     XMM1, 23

        PACKSSDW  XMM0, XMM1
        PACKUSWB  XMM0, XMM5
        MOVD      EAX, XMM0
        OR        EAX, $FF000000
        RET
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE41_8081
//
// SSE4.1 version of BlendReg_SSE2_8081 using PMOVZXBD.
//------------------------------------------------------------------------------
function BlendReg_SSE41_8081(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) onto a background color (B),
  // using alpha channel value of F
  // Result Z = (Fa * Fargb + (255 - Fa) * Bargb + 128) / 255
  // Bit-perfect formula: Round(x / 255) = ((x * $8081 + $003FFF7F) shr 23)

{$IFDEF TARGET_x86}
        MOVD      XMM0, EAX
        MOVD      XMM1, EDX
        PMOVZXBD  XMM0, XMM0          // components of F (dword)
        PMOVZXBD  XMM1, XMM1          // components of B (dword)

        PSHUFD    XMM2, XMM0, $FF     // Broadcast Fa

        MOV       EAX, $00FF
        MOVD      XMM3, EAX
        PSHUFD    XMM3, XMM3, 0       // 255
        PSUBD     XMM3, XMM2          // 255 - Fa

        PMULLD    XMM0, XMM2          // Fa * F
        PMULLD    XMM1, XMM3          // (255 - Fa) * B
        PADDD     XMM0, XMM1          // x

        MOV       EAX, $8081
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PMULLD    XMM0, XMM2          // x * $8081

        MOV       EAX, $003FFF7F      // Bias
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PADDD     XMM0, XMM2
        PSRLD     XMM0, 23

        PXOR      XMM5, XMM5
        PACKUSDW  XMM0, XMM5          // dword -> word
        PACKUSWB  XMM0, XMM5          // word -> byte
        MOVD      EAX, XMM0
        OR        EAX, $FF000000
        RET
{$ENDIF}

{$IFDEF TARGET_x64}
        MOVD      XMM0, ECX
        MOVD      XMM1, EDX
        PMOVZXBD  XMM0, XMM0
        PMOVZXBD  XMM1, XMM1

        PSHUFD    XMM2, XMM0, $FF     // Fa

        MOV       EAX, $00FF
        MOVD      XMM3, EAX
        PSHUFD    XMM3, XMM3, 0
        PSUBD     XMM3, XMM2          // 255 - Fa

        PMULLD    XMM0, XMM2
        PMULLD    XMM1, XMM3
        PADDD     XMM0, XMM1          // x

        MOV       EAX, $8081
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PMULLD    XMM0, XMM2          // * $8081

        MOV       EAX, $003FFF7F      // Bias
        MOVD      XMM2, EAX
        PSHUFD    XMM2, XMM2, 0
        PADDD     XMM0, XMM2
        PSRLD     XMM0, 23

        PXOR      XMM5, XMM5
        PACKUSDW  XMM0, XMM5
        PACKUSWB  XMM0, XMM5
        MOVD      EAX, XMM0
        OR        EAX, $FF000000
        RET
{$ENDIF}
end;

//------------------------------------------------------------------------------
// BlendReg_SSE41_Sanyin
//
// Sanyin's BlendReg using the `(x + (x shr 8)) shr 8`  formula for lossless
// div255 calculation.
//------------------------------------------------------------------------------
procedure SSE_80008000_ALIGNED; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
{$ifdef FPC}
  ALIGN 16
{$else}
  .ALIGN 16
{$endif}
  dw $8000, $8000, $8000, $8000
  dw $8000, $8000, $8000, $8000
end;

function BlendReg_SSE41_Sanyin(F, B: TColor32): TColor32; {$IFDEF FPC} assembler; nostackframe; {$ENDIF}
asm
  // blend foreground color (F) onto a background color (B),
  // using alpha channel value of F
  // Result Z = (Fa * Fargb + (255 - Fa) * Bargb + 128) / 255
  //  t = x + 128;
  // Result = (t + (t shr 8)) shr 8;
{$IFDEF TARGET_x64}
        MOVD      XMM0, ECX
        MOVD      XMM1, EDX
{$ENDIF}
{$IFDEF TARGET_x86}
        MOVD      XMM0, EAX
        MOVD      XMM1, EDX
{$ENDIF}
        PMOVZXBW  XMM0, XMM0          // Components of F (word)
        PMOVZXBW  XMM1, XMM1          // Components of B (word)
        //PSHUFLW   XMM2, XMM0, $FF     // Broadcast Fa into words
        PSHUFD XMM2,XMM0,$FF // faster?
        MOVDQA    XMM3, DQWORD PTR [SSE_FF00FF00_ALIGNED]
        // Make $00FF
        //PCMPEQD XMM3, XMM3            // $FFFFFFFF
        //PSRLW   XMM3, 8               // $00FF
        PSUBW     XMM3, XMM2          // 255 - Fa
        PMULLW    XMM0, XMM2          // Fa * F
        PMULLW    XMM1, XMM3          // (255 - Fa) * B
        PADDW     XMM0, XMM1          // x = Fa * F + (255 - Fa) * B
        PADDW     XMM0, DQWORD PTR [SSE_80008000_ALIGNED]        // x + 128
        MOVDQA    XMM1, XMM0
        PSRLW     XMM1, 8
        PADDW     XMM0, XMM1
        PSRLW     XMM0, 8
        PACKUSWB  XMM0, XMM0          // word -> byte
        MOVD      EAX, XMM0
        OR        EAX, $FF000000
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

{-$define TEST_OPAQUE}

procedure BM_BlendReg(const state: TState);
begin
  var BlendRegProc: TBlendReg := TBlendReg(state[0]);

  var BoundaryTest := ' Boundary: ';
  if (BlendRegProc(0, clRed32) = clRed32) then
    BoundaryTest := BoundaryTest + 'Pass'
  else
    BoundaryTest := BoundaryTest + 'Fail';
  if (BlendRegProc(clBlue32, clRed32) = clBlue32) then
    BoundaryTest := BoundaryTest + ', Pass'
  else
    BoundaryTest := BoundaryTest + ', Fail';
  state.SetLabel(BoundaryTest);

  for var _ in state do
  begin
{$ifdef TEST_OPAQUE} // Benchmark blend opaque onto opaque
    var Alpha := 255;
    for var xxx := 0 to 255 do
{$else}
    for var Alpha := 0 to 255 do
{$endif}
      for var F := 0 to $FF do
      begin
        var ColorFG: TColor32 := (Alpha shl 24) or F or (F shl 8) or (F shl 16);
        for var B := 0 to $FF do
        begin
          var ColorBG: TColor32 := $FF000000 or B or (B shl 8) or (B shl 16);
          BlendRegProc(ColorFG, ColorBG);
        end;
      end;
  end;
end;

procedure Main;
begin
  Spring.Benchmark.benchmark_format_args := False;

  var Binding := BlendRegistry.FindBinding('BlendReg');
  Assert(Binding <> nil);

  for var Implement in Binding do
  begin
    var bm := Spring.Benchmark.Benchmark(BM_BlendReg, Implement.Name).Args([Int64(Implement.Proc)]);
    bm.TimeUnit(kMillisecond);
    bm.MinTime(10); // seconds
  end;

  Spring.Benchmark.Benchmark_Main;
end;

begin
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE41_Sanyin, [isSSE2], 1).Name := 'BlendReg_SSE41_Sanyin';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE41_8081, [isSSE2], 1).Name := 'BlendReg_SSE41_8081';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE2_8081, [isSSE2], 1).Name := 'BlendReg_SSE2_8081';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE41_No_bias_ptr, [isSSE2], 1).Name := 'BlendReg_SSE41_No_bias_ptr';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE2_No_bias_ptr, [isSSE2], 1).Name := 'BlendReg_SSE2_No_bias_ptr';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE2_BoundaryFixes, [isSSE2], 1).Name := 'BlendReg_SSE2_BoundaryFixes';
  BlendRegistry[@@BlendReg].Add(@BlendReg_SSE2_Org, [isSSE2], 1).Name := 'BlendReg_SSE2 (Original)';
  try
    Main;
    WriteLn('Done');
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

