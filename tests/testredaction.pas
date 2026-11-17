unit TestRedaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, fpcunit, testregistry, BaseTest, ZbUtility;

type
  TTestRedaction = class(TBaseTestCase)
  published
    procedure TestTheArmenianLigatureEchoIsMasked;
    procedure TestTheGreekPsiliIotaEchoIsMasked;
    procedure TestTheGeneratedTableKeepsTheOldFolds;
    procedure TestASecretInsideTheMaskIsNotRescanned;
    procedure TestMaskingIsIdempotent;
    procedure TestALargeBodyIsMaskedQuickly;
    procedure TestALargeBodyWithAShortSecretIsMaskedQuickly;
    procedure TestALargeEscapedBodyIsMaskedQuickly;
    procedure TestALargeNonAsciiBodyIsMaskedQuickly;
    procedure TestADecodedSpellingAtTheEndOfALongBodyIsStillCaught;
  end;

implementation

function Repeated(const Piece: String; Times: Integer): String;
var
  I: Integer;
  Builder: TStringStream;
begin
  Builder := TStringStream.Create('');
  try
    for I := 1 to Times do
      Builder.WriteString(Piece);
    Result := Builder.DataString;
  finally
    Builder.Free;
  end;
end;

procedure TTestRedaction.TestTheArmenianLigatureEchoIsMasked;
var
  Key: String;
begin
  { U+0587 folds to two letters: the capitals an upper-casing echo has
    (U+0535 U+0552) and the small ones (U+0565 U+0582) are the same text. }
  Key := 'k-' + #$D6#$87 + '-9';
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$D4#$B5#$D5#$92 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a K-' + #$D5#$A5#$D6#$82 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$D6#$87 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$D4#$B5#$D5#$92 + '-9 b', 'K-' + #$D5#$A5#$D6#$82 + '-9'));
end;

procedure TTestRedaction.TestTheGreekPsiliIotaEchoIsMasked;
var
  Key: String;
begin
  { U+1F80 folds to U+1F00 U+03B9; its upper-cased echo is U+1F08 U+0399
    (or the titlecase letter U+1F88). }
  Key := 'k-' + #$E1#$BE#$80 + '-9';
  AssertEquals('a REDACTED b', ZbMaskSecret('a K-' + #$E1#$BC#$88#$CE#$99 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$E1#$BE#$88 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$E1#$BC#$80#$CE#$B9 + '-9 b', Key));
  AssertEquals('a REDACTED b', ZbMaskSecret('a k-' + #$E1#$BE#$80 + '-9 b', 'K-' + #$E1#$BC#$88#$CE#$99 + '-9'));
end;

{ Whether Echo, in any case, is masked as a spelling of Secret. }
function Masks(const Echo, Secret: String): Boolean;
begin
  Result := ZbMaskSecret('x ' + Echo + ' y', Secret) = 'x REDACTED y';
end;

procedure TTestRedaction.TestTheGeneratedTableKeepsTheOldFolds;
begin
  AssertTrue('sharp s', Masks('STRASSE', 'stra' + #$C3#$9F + 'e'));
  AssertTrue('capital sharp s', Masks('strasse', 'STRA' + #$E1#$BA#$9E + 'E'));
  AssertTrue('Kelvin', Masks('k', #$E2#$84#$AA));
  AssertTrue('Kelvin echo', Masks(#$E2#$84#$AA, 'K'));
  AssertTrue('dotted capital I', Masks('i', #$C4#$B0));
  AssertTrue('dotless i', Masks('I', #$C4#$B1));
  AssertTrue('final sigma', Masks(#$CE#$A3, #$CF#$82));
  AssertTrue('final sigma echo', Masks(#$CF#$82, #$CE#$A3));
  AssertTrue('ffi ligature', Masks('FFI', #$EF#$AC#$83));
  AssertTrue('st ligature', Masks('ST', #$EF#$AC#$86));
  AssertTrue('Hangul', Masks(#$E1#$84#$80#$E1#$85#$A1, #$EA#$B0#$80));
  AssertTrue('accent', Masks('e', #$C3#$89));
  AssertTrue('U+0587', Masks(#$D4#$B5#$D5#$92, #$D6#$87));
  AssertTrue('U+1F80', Masks(#$E1#$BC#$88#$CE#$99, #$E1#$BE#$80));
  AssertTrue('ASCII', Masks('ABC', 'aBc'));
  AssertFalse('different letters', Masks('abd', 'abc'));
end;

procedure TTestRedaction.TestASecretInsideTheMaskIsNotRescanned;
var
  Secrets: array[0..5] of String;
  I: Integer;
begin
  Secrets[0] := 'a';
  Secrets[1] := 'e';
  Secrets[2] := 'act';
  Secrets[3] := 'ted';
  Secrets[4] := 'RED';
  Secrets[5] := 'CTE';
  for I := 0 to High(Secrets) do
  begin
    AssertEquals(Secrets[I], 'x REDACTED y', ZbMaskSecret('x ' + Secrets[I] + ' y', Secrets[I]));
    { Never the omitted-body text, and each hit is one mask. }
    AssertTrue(Secrets[I], ZbMaskSecret('plain text and ' + Secrets[I] + ' ' + Secrets[I], Secrets[I]) <>
      ZB_SECRET_OMITTED_MESSAGE);
  end;
  AssertEquals('hREDACTEDllo REDACTED', ZbMaskSecret('hello e', 'e'));
  AssertEquals('REDACTEDREDACTED', ZbMaskSecret('aa', 'a'));
  { A secret that only touches a literal 'REDACTED' is still masked. }
  AssertEquals('REDACTED', ZbMaskSecret('KEYREDACTED', 'keyredacted'));
end;

procedure TTestRedaction.TestMaskingIsIdempotent;
var
  Secrets: array[0..3] of String;
  Once: String;
  I: Integer;
begin
  Secrets[0] := 'e';
  Secrets[1] := 'ted';
  Secrets[2] := 'a';
  Secrets[3] := 'act';
  for I := 0 to High(Secrets) do
  begin
    Once := ZbMaskSecret('an echo of ' + Secrets[I] + ' and again ' + Secrets[I], Secrets[I]);
    AssertEquals(Secrets[I], Once, ZbMaskSecret(Once, Secrets[I]));
  end;
end;

procedure TTestRedaction.TestALargeBodyIsMaskedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  Body := Repeated('{"message":"the quick brown fox jumps over the lazy dog","n":12345} ', 45000) +
    ' key-abc-123-XYZ ';
  AssertTrue('body is at least 2.5 MB', Length(Body) > 2500000);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'key-abc-123-xyz');
  AssertTrue('masking a 2.8 MB body took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms',
    MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('the key is masked', Pos('REDACTED', Masked) > 0);
  AssertEquals('no key left', 0, Pos('key-abc', Masked));
end;

procedure TTestRedaction.TestALargeBodyWithAShortSecretIsMaskedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  Body := Repeated('the quick brown fox jumps over the lazy dog. ', 60000);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'ted');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('the text is kept', Pos('the quick brown fox', Masked) = 1);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'e');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('every e is masked', Pos('thREDACTED quick', Masked) = 1);
  AssertTrue('not the omitted text', Masked <> ZB_SECRET_OMITTED_MESSAGE);
end;

procedure TTestRedaction.TestALargeEscapedBodyIsMaskedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  { Characters that make a decoded view: %, +, \ and &. }
  Body := Repeated('{\"a\":\"x%20y+z &amp; q\\n\"} ', 100000);
  AssertTrue('body is at least 2.5 MB', Length(Body) > 2500000);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'key-abc-123-xyz');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('unchanged', Masked = Body);
end;

procedure TTestRedaction.TestALargeNonAsciiBodyIsMaskedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  Body := Repeated('caf' + #$C3#$A9 + ' stra' + #$C3#$9F + 'e ' + #$E1#$BE#$80 + ' ', 150000);
  AssertTrue('body is at least 2.5 MB', Length(Body) > 2500000);
  Started := Now;
  Masked := ZbMaskSecret(Body + 'K-STRASSE-77', 'k-stra' + #$C3#$9F + 'e-77');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertEquals('masked at the end', 'REDACTED', Copy(Masked, Length(Masked) - 7, 8));
end;

procedure TTestRedaction.TestADecodedSpellingAtTheEndOfALongBodyIsStillCaught;
var
  Body: String;
begin
  Body := Repeated('plain filler text. ', 20000);
  AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret(Body + 'ab%5Cx27cd-9z', 'ab' + #39 + 'cd-9z'));
  AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('ab%5Cx27cd-9z' + Body, 'ab' + #39 + 'cd-9z'));
  { The forms are masked wherever they are. }
  AssertEquals(Body + 'REDACTED' + Body,
    ZbMaskSecret(Body + 'ab-cd-9z' + Body, 'AB-CD-9Z'));
end;

initialization
  RegisterTest(TTestRedaction);
end.
