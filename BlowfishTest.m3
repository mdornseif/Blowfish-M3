MODULE BlowfishTest EXPORTS Main;

(* BlowfishTest by drt@ailis.de					  *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: BlowfishTest.m3,v 1.1.1.2 2000/04/11 09:28:27 drt Exp $ *)

(* This is a little ugly test for my Blowfish routines *)

(* $Log: BlowfishTest.m3,v $
 * Revision 1.1.1.2  2000/04/11 09:28:27  drt
 * Version with chaining
 *
(* Revision 1.1.1.1  1999/12/14 14:58:47  drt
(* initial revision
(*
(* Revision 1.2  1999/07/22 06:53:17  drt
(* Testing the new objectoriented Interface
(*
 * Revision 1.1  1999/07/12 09:07:51  drt
 * Initial revision
 * *)

IMPORT Word;
IMPORT IO;
IMPORT Fmt;
IMPORT Blowfish;

VAR 
  PL, PR, CL, CR: Word.T;
  blow: Blowfish.T := NEW(Blowfish.T).init("abcdefghijklmnopqrstuvwxyz");

CONST
  rcsid = "$Id: BlowfishTest.m3,v 1.1.1.2 2000/04/11 09:28:27 drt Exp $";

BEGIN

  PL := 16_424c4f57;
  PR := 16_46495348; 
  
  IO.Put("P          : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");

  blow.encipher( PL, PR );

  IO.Put("           : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");

  IO.Put("should read: 16_324ed0fe 16_f413a203\n");

  blow.decipher( PL, PR );
  
  IO.Put("P          : 16_" & Fmt.Unsigned(PL, 16));
  IO.Put(" 16_" & Fmt.Unsigned(PR, 16) & "\n");
  
END BlowfishTest.
