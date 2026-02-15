(***************************************************************************
 * Minimal SHA-256 implementation for Trndi (PKCE code challenge support)
 *
 * SPDX-License-Identifier: LGPL-3.0-only
 *
 * NOTE: This implementation is intentionally small and self-contained.
 ***************************************************************************)
unit slicke.sha256;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  TSHA256Digest = array[0..31] of byte;

function SHA256String(const S: string): TSHA256Digest;

implementation

type
  TWordArray64 = array[0..63] of LongWord;

const
  K: TWordArray64 = (
    $428A2F98,$71374491,$B5C0FBCF,$E9B5DBA5,$3956C25B,$59F111F1,$923F82A4,$AB1C5ED5,
    $D807AA98,$12835B01,$243185BE,$550C7DC3,$72BE5D74,$80DEB1FE,$9BDC06A7,$C19BF174,
    $E49B69C1,$EFBE4786,$0FC19DC6,$240CA1CC,$2DE92C6F,$4A7484AA,$5CB0A9DC,$76F988DA,
    $983E5152,$A831C66D,$B00327C8,$BF597FC7,$C6E00BF3,$D5A79147,$06CA6351,$14292967,
    $27B70A85,$2E1B2138,$4D2C6DFC,$53380D13,$650A7354,$766A0ABB,$81C2C92E,$92722C85,
    $A2BFE8A1,$A81A664B,$C24B8B70,$C76C51A3,$D192E819,$D6990624,$F40E3585,$106AA070,
    $19A4C116,$1E376C08,$2748774C,$34B0BCB5,$391C0CB3,$4ED8AA4A,$5B9CCA4F,$682E6FF3,
    $748F82EE,$78A5636F,$84C87814,$8CC70208,$90BEFFFA,$A4506CEB,$BEF9A3F7,$C67178F2
  );

function ROR32(const X: LongWord; const N: LongWord): LongWord; inline;
begin
  Result := (X shr N) or (X shl (32 - N));
end;

function Ch(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := (X and Y) xor ((not X) and Z);
end;

function Maj(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := (X and Y) xor (X and Z) xor (Y and Z);
end;

function BigSigma0(const X: LongWord): LongWord; inline;
begin
  Result := ROR32(X, 2) xor ROR32(X, 13) xor ROR32(X, 22);
end;

function BigSigma1(const X: LongWord): LongWord; inline;
begin
  Result := ROR32(X, 6) xor ROR32(X, 11) xor ROR32(X, 25);
end;

function SmallSigma0(const X: LongWord): LongWord; inline;
begin
  Result := ROR32(X, 7) xor ROR32(X, 18) xor (X shr 3);
end;

function SmallSigma1(const X: LongWord): LongWord; inline;
begin
  Result := ROR32(X, 17) xor ROR32(X, 19) xor (X shr 10);
end;

function SHA256String(const S: string): TSHA256Digest;
var
  data: TBytes;
  bitLen: QWord;
  newLen, i, t, origLen: SizeInt;
  Hash: array[0..7] of LongWord;
  W: TWordArray64;
  a, b, c, d, e, f, g, hh: LongWord;
  T1, T2: LongWord;
  blockStart: SizeInt;
  b0, b1, b2, b3: LongWord;
begin
  // Initial hash values
  Hash[0] := $6A09E667; Hash[1] := $BB67AE85; Hash[2] := $3C6EF372; Hash[3] := $A54FF53A;
  Hash[4] := $510E527F; Hash[5] := $9B05688C; Hash[6] := $1F83D9AB; Hash[7] := $5BE0CD19;

  data := BytesOf(S);
  origLen := Length(data);
  bitLen := QWord(origLen) * 8;

  // Pad: append 0x80 then zeros until length mod 64 = 56
  newLen := origLen + 1;
  while (newLen mod 64) <> 56 do
    Inc(newLen);

  SetLength(data, newLen + 8);
  data[origLen] := $80;
  for i := origLen + 1 to newLen - 1 do
    data[i] := 0;

  data[newLen + 0] := (bitLen shr 56) and $FF;
  data[newLen + 1] := (bitLen shr 48) and $FF;
  data[newLen + 2] := (bitLen shr 40) and $FF;
  data[newLen + 3] := (bitLen shr 32) and $FF;
  data[newLen + 4] := (bitLen shr 24) and $FF;
  data[newLen + 5] := (bitLen shr 16) and $FF;
  data[newLen + 6] := (bitLen shr 8) and $FF;
  data[newLen + 7] := (bitLen) and $FF;

  blockStart := 0;
  while blockStart < Length(data) do
  begin
    // Prepare message schedule W
    for t := 0 to 15 do
    begin
      b0 := data[blockStart + (t * 4) + 0];
      b1 := data[blockStart + (t * 4) + 1];
      b2 := data[blockStart + (t * 4) + 2];
      b3 := data[blockStart + (t * 4) + 3];
      W[t] := (b0 shl 24) or (b1 shl 16) or (b2 shl 8) or b3;
    end;
    for t := 16 to 63 do
      W[t] := LongWord(
        (QWord(SmallSigma1(W[t-2])) + QWord(W[t-7]) + QWord(SmallSigma0(W[t-15])) + QWord(W[t-16]))
        and $FFFFFFFF
      );

    a := Hash[0]; b := Hash[1]; c := Hash[2]; d := Hash[3];
    e := Hash[4]; f := Hash[5]; g := Hash[6]; hh := Hash[7];

    for t := 0 to 63 do
    begin
      T1 := LongWord(
        (QWord(hh) + QWord(BigSigma1(e)) + QWord(Ch(e, f, g)) + QWord(K[t]) + QWord(W[t]))
        and $FFFFFFFF
      );
      T2 := LongWord(
        (QWord(BigSigma0(a)) + QWord(Maj(a, b, c)))
        and $FFFFFFFF
      );
      hh := g;
      g := f;
      f := e;
      e := LongWord((QWord(d) + QWord(T1)) and $FFFFFFFF);
      d := c;
      c := b;
      b := a;
      a := LongWord((QWord(T1) + QWord(T2)) and $FFFFFFFF);
    end;

    Hash[0] := LongWord((QWord(Hash[0]) + QWord(a)) and $FFFFFFFF);
    Hash[1] := LongWord((QWord(Hash[1]) + QWord(b)) and $FFFFFFFF);
    Hash[2] := LongWord((QWord(Hash[2]) + QWord(c)) and $FFFFFFFF);
    Hash[3] := LongWord((QWord(Hash[3]) + QWord(d)) and $FFFFFFFF);
    Hash[4] := LongWord((QWord(Hash[4]) + QWord(e)) and $FFFFFFFF);
    Hash[5] := LongWord((QWord(Hash[5]) + QWord(f)) and $FFFFFFFF);
    Hash[6] := LongWord((QWord(Hash[6]) + QWord(g)) and $FFFFFFFF);
    Hash[7] := LongWord((QWord(Hash[7]) + QWord(hh)) and $FFFFFFFF);

    Inc(blockStart, 64);
  end;

  // Output digest (big-endian)
  for i := 0 to 7 do
  begin
    Result[i*4 + 0] := (Hash[i] shr 24) and $FF;
    Result[i*4 + 1] := (Hash[i] shr 16) and $FF;
    Result[i*4 + 2] := (Hash[i] shr 8) and $FF;
    Result[i*4 + 3] := (Hash[i]) and $FF;
  end;
end;

end.
