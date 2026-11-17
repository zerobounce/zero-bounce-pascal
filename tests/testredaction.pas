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
    procedure TestAUrlEncodedSecretInTheMiddleOfALargeBodyIsCaught;
    procedure TestAJsonEscapedSecretInTheMiddleOfALargeBodyIsCaught;
    procedure TestAnHtmlEntitySecretInTheMiddleOfALargeBodyIsCaught;
    procedure TestAThreeLayerSecretInTheMiddleOfALargeBodyIsCaught;
    procedure TestALargeBodyWithEveryDecoderTriggerIsMaskedQuickly;
    procedure TestAMatchReachingIntoAProtectedLiteralIsMaskedWithIt;
    procedure TestAPartlyEncodedSecretWithTheMaskTextInItIsStillFound;
    procedure TestPrecomposedAndDecomposedFormsAreOneTextInAnyCase;
    procedure TestMarksAreKeptSoALetterIsNotItsAccentedForm;
    procedure TestMarksInAnotherOrderAreTheSameText;
    procedure TestATurkishUpperCasedKeyIsMasked;
    procedure TestALongRunOfMarksIsFoldedQuickly;
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
  AssertTrue('dotted capital I as i and a dot', Masks('i' + #$CC#$87, #$C4#$B0));
  AssertTrue('dotless i', Masks('I', #$C4#$B1));
  AssertTrue('final sigma', Masks(#$CE#$A3, #$CF#$82));
  AssertTrue('final sigma echo', Masks(#$CF#$82, #$CE#$A3));
  AssertTrue('ffi ligature', Masks('FFI', #$EF#$AC#$83));
  AssertTrue('st ligature', Masks('ST', #$EF#$AC#$86));
  AssertTrue('Hangul', Masks(#$E1#$84#$80#$E1#$85#$A1, #$EA#$B0#$80));
  AssertFalse('a plain e is not an e acute', Masks('e', #$C3#$89));
  AssertTrue('accent', Masks('e' + #$CC#$81, #$C3#$89));
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

{ A body of at least 2.5 MB with Spelling in the middle, between two halves of
  filler: the old head and tail cut-off never looked there. }
function InTheMiddle(const Spelling: String): String;
var
  Half: String;
begin
  Half := Repeated('{"message":"the quick brown fox jumps over the lazy dog","n":12345} ', 22000);
  Result := Half + ' ' + Spelling + ' ' + Half;
end;

procedure AssertCaughtInTheMiddle(const Spelling, Secret: String);
var
  Body: String;
  Started: TDateTime;
begin
  Body := InTheMiddle(Spelling);
  TAssert.AssertTrue('body is at least 2.5 MB', Length(Body) > 2500000);
  Started := Now;
  TAssert.AssertTrue('the body is omitted, not returned with the secret in it',
    ZbMaskSecret(Body, Secret) = ZB_SECRET_OMITTED_MESSAGE);
  TAssert.AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
end;

procedure TTestRedaction.TestAUrlEncodedSecretInTheMiddleOfALargeBodyIsCaught;
begin
  { Two URL layers: %252D is %2D is a hyphen. }
  AssertCaughtInTheMiddle('ab%252Dcd-9z', 'ab-cd-9z');
end;

procedure TTestRedaction.TestAJsonEscapedSecretInTheMiddleOfALargeBodyIsCaught;
begin
  AssertCaughtInTheMiddle('ab\u002dcd-9z', 'ab-cd-9z');
end;

procedure TTestRedaction.TestAnHtmlEntitySecretInTheMiddleOfALargeBodyIsCaught;
begin
  AssertCaughtInTheMiddle('ab&#45;cd-9z', 'ab-cd-9z');
end;

procedure TTestRedaction.TestAThreeLayerSecretInTheMiddleOfALargeBodyIsCaught;
begin
  { JSON \u002d, its backslash as an HTML reference, and that URL-encoded. }
  AssertCaughtInTheMiddle('ab%26%2392%3Bu002dcd-9z', 'ab-cd-9z');
end;

procedure TTestRedaction.TestALargeBodyWithEveryDecoderTriggerIsMaskedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  { %, +, &, a backslash and non-ASCII text in one 2.8 MB body: each layer of
    the decoded views changes the text and all four are tried. }
  Body := Repeated('a%20b+c &amp; d\u0041 caf' + #$C3#$A9 + ' &#233; ', 100000);
  AssertTrue('body is at least 2.5 MB', Length(Body) > 2500000);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'key-abc-123-xyz');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('unchanged', Masked = Body);
end;

{ Security L: a match that overlaps "REDACTED" or the omitted-body text is
  masked together with it, not lost on the rebuild. }
procedure TTestRedaction.TestAMatchReachingIntoAProtectedLiteralIsMaskedWithIt;
begin
  { Starting inside the literal and ending after it. }
  AssertEquals('REDACTED', ZbMaskSecret('REDACTEDxyz1', 'ACTEDxyz1'));
  AssertEquals('x REDACTED y', ZbMaskSecret('x REDACTED12 y', 'DACTED12'));
  { Ending inside it, starting before it. }
  AssertEquals('x REDACTED y', ZbMaskSecret('x pre-REDACTED y', 'pre-REDA'));
  { Around it, and in the omitted-body text. }
  AssertEquals('REDACTED', ZbMaskSecret('aREDACTEDb', 'aREDACTEDb'));
  AssertEquals('x REDACTED y', ZbMaskSecret('x ' + ZB_SECRET_OMITTED_MESSAGE + 'k123 y',
    'credential.k123'));
  AssertEquals('x REDACTED y', ZbMaskSecret('x ' + ZB_SECRET_OMITTED_MESSAGE + 'k123 y',
    'was omitted because it contained a credential.k123'));
  { Idempotent, and a match wholly inside the literal is still left alone. }
  AssertEquals('REDACTED', ZbMaskSecret('REDACTED', 'REDACTED'));
  AssertEquals('x REDACTED y', ZbMaskSecret('x REDACTED y', 'DACT'));
  AssertEquals('REDACTED', ZbMaskSecret(ZbMaskSecret('REDACTEDxyz1', 'ACTEDxyz1'), 'ACTEDxyz1'));
end;

{ Security L: the views are looked at with the literal in them, so a secret
  that has REDACTED in it and is partly encoded is found. }
procedure TTestRedaction.TestAPartlyEncodedSecretWithTheMaskTextInItIsStillFound;
begin
  AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo tokenREDACTED%39%39 end', 'tokenREDACTED99'));
  AssertEquals(ZB_SECRET_OMITTED_MESSAGE, ZbMaskSecret('echo token%52EDACTED99 end', 'tokenREDACTED99'));
  AssertEquals('plain REDACTED text', ZbMaskSecret('plain REDACTED text', 'tokenREDACTED99'));
end;

{ Security M: fold(x) = NFD(casefold(NFD(x))). U+1F84 is U+1F04 and an iota
  subscript; U+1F80 with an acute after it reorders to the same marks. }
procedure TTestRedaction.TestPrecomposedAndDecomposedFormsAreOneTextInAnyCase;
begin
  AssertTrue('U+1F84 against U+1F80 and U+0301', Masks(#$E1#$BE#$84, #$E1#$BE#$80 + #$CC#$81));
  AssertTrue('U+1F80 and U+0301 against U+1F84', Masks(#$E1#$BE#$80 + #$CC#$81, #$E1#$BE#$84));
  AssertTrue('U+1F84 upper-cased', Masks(#$E1#$BC#$8C#$CE#$99, #$E1#$BE#$84));
  { U+037E (Greek question mark) is a semicolon. }
  AssertTrue('U+037E as ;', Masks(';', #$CD#$BE));
  AssertTrue('; as U+037E', Masks(#$CD#$BE, ';'));
  { CJK compatibility ideograph U+F900 is U+8C48. }
  AssertTrue('U+F900 as U+8C48', Masks(#$E8#$B1#$88, #$EF#$A4#$80));
  { U+01E2 (AE with macron) and U+01E3, its lower case. }
  AssertTrue('AE with macron, lower case', Masks(#$C7#$A3, #$C7#$A2));
  AssertTrue('Hangul syllable as jamo', Masks(#$E1#$84#$80#$E1#$85#$A1, #$EA#$B0#$80));
  AssertTrue('jamo as Hangul syllable', Masks(#$EA#$B0#$80, #$E1#$84#$80#$E1#$85#$A1));
end;

procedure TTestRedaction.TestMarksAreKeptSoALetterIsNotItsAccentedForm;
begin
  AssertFalse('e is not e acute', Masks('e', #$C3#$A9));
  AssertFalse('e acute is not e', Masks(#$C3#$A9, 'e'));
  AssertFalse('e is not e and an acute', Masks('e', 'e' + #$CC#$81));
  AssertTrue('e acute is e and an acute', Masks('e' + #$CC#$81, #$C3#$A9));
  AssertTrue('E acute is e and an acute', Masks('E' + #$CC#$81, #$C3#$A9));
  { A secret that is followed by a mark in the text is another text. }
  AssertEquals('xkey' + #$CC#$87 + 'y', ZbMaskSecret('xkey' + #$CC#$87 + 'y', 'key'));
  AssertEquals('x REDACTED y', ZbMaskSecret('x key y', 'key'));
end;

procedure TTestRedaction.TestATurkishUpperCasedKeyIsMasked;
begin
  AssertEquals('body REDACTED end', ZbMaskSecret('body ZBKEY-' + #$C4#$B0 + '1 end', 'zbkey-i1'));
  AssertEquals('body REDACTED end', ZbMaskSecret('body ZBKEY-I1 end', 'zbkey-i1'));
  AssertEquals('body REDACTED end', ZbMaskSecret('body zbkey-i' + #$CC#$87 + '1 end', 'zbkey-i1'));
  AssertEquals('body REDACTED end', ZbMaskSecret('body ZBKEY-' + #$C4#$B1 + '1 end', 'zbkey-i1'));
end;

procedure TTestRedaction.TestMarksInAnotherOrderAreTheSameText;
var
  Below, Above: String;
begin
  Below := #$CC#$A3;  { U+0323, class 220 }
  Above := #$CC#$81;  { U+0301, class 230 }
  AssertTrue('above then below against below then above',
    Masks('a' + Above + Below + 'b', 'a' + Below + Above + 'b'));
  AssertTrue('the other way',
    Masks('a' + Below + Above + 'b', 'a' + Above + Below + 'b'));
  { U+1E0D is d with a dot below; with an acute after it: another order again. }
  AssertTrue('precomposed dot below, then an acute',
    Masks('d' + Above + Below, #$E1#$B8#$8D + Above));
  { Marks of one class keep their order. }
  AssertFalse('same class, another order',
    Masks('a' + #$CC#$81 + #$CC#$80 + 'b', 'a' + #$CC#$80 + #$CC#$81 + 'b'));
end;

procedure TTestRedaction.TestALongRunOfMarksIsFoldedQuickly;
var
  Body, Masked: String;
  Started: TDateTime;
begin
  { 600,000 marks in decreasing class order, which the ordering has to
    reverse, and many short runs out of order. }
  Body := 'a' + Repeated(#$CC#$81#$CC#$A3, 300000) + ' ' + Repeated('x' + #$CC#$81#$CC#$A3, 100000);
  Started := Now;
  Masked := ZbMaskSecret(Body, 'key-abc-123-xyz');
  AssertTrue('took ' + IntToStr(MilliSecondsBetween(Now, Started)) + ' ms', MilliSecondsBetween(Now, Started) < 3000);
  AssertTrue('unchanged', Masked = Body);
end;

initialization
  RegisterTest(TTestRedaction);
end.
