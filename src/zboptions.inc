{
    This file includes compile options and other customizations.
}

{$H+}

{$IFNDEF FPC}
    {$DEFINE DCC}   // if not using FPC then DCC compiler is used (Delphi/BCB)
                    // others are not supported
{$ENDIF}

{$IFDEF DCC}
  {$DEFINE DELPHI}
{$ENDIF}

{ More compiler options }

{$IFDEF FPC}               // Free Pascal options - some options set above (like min enum size)
                           // are reset to defaults by setting {$MODE} so they are
                           // redeclared here
  {$MODE DELPHI}           // compatible with delphi
{$ENDIF}

{$IFDEF HAS_INLINE}
  {$INLINE ON}             // turns inlining on for compilers that support it
{$ENDIF}


