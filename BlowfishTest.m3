MODULE BlowfishTest EXPORTS Main;

(* BlowfishTest by drt@ailis.de					  *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: BlowfishTest.m3,v 1.1 2000/04/11 08:34:48 drt Exp $ *)

(* This is a little ugly test for my Blowfish routines *)

(* $Log: BlowfishTest.m3,v $
 * Revision 1.1  2000/04/11 08:34:48  drt
 * Initial revision
 *
 * Revision 1.1  1999/07/12 09:07:51  drt
 * Initial revision
 * *)

IMPORT Word;
IMPORT IO;
IMPORT Fmt;
IMPORT Blowfish;

VAR 
  PL, PR, CL, CR : Word.T;

BEGIN
  
  Blowfish.Init( "abcdefghijklmnopqrstuvwxyz" ); 

  PL := 16_424c4f57;
  PR := 16_46495348; 
  
  IO.Put("PT         : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");

  Blowfish.Encipher( PL, PR );

  IO.Put("CT         : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");

  IO.Put("should read: 16_324ed0fe 16_f413a203\n");

  Blowfish.Decipher( PL, PR );
  
  IO.Put("PT         : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");

  FOR PL := 1 TO 16_ffffff DO
    FOR PR := 1 TO 16_ffffff BY 16_ffffff DO
      CL := PL;
      CR := PR;
      Blowfish.Encipher( CL, CR );
    END;
  END;
END BlowfishTest.
