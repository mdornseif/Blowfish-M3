MODULE BlowfishCBC;

(* BlowfishCBC by drt@ailis.de					*)
(*                (K) 1999 all rights reversed      		*)

(* $Id: BlowfishCBC.m3,v 1.1 2000/04/11 09:28:27 drt Exp $ *)

(* CBC mode for Blowfish  					*)
(* The Features of CBC (Cipher Block Chaining Mode) are: 	*)
(* - a multiple of 64 bits are enciphered at a time.		*)
(* - The CBC mode produces the same ciphertext whenever the 	*)
(*   same plaintext is encrypted using the same key and		*)
(*   starting variable.						*)
(* - The chaining operation makes the ciphertext blocks 	*)
(*   dependent on the current and all preceding plaintext  	*)
(*   blocks and therefore blocks can not be rearranged.		*)
(* - The use of different starting variables prevents the 	*)
(*   same plaintext enciphering to the same ciphertext.		*)
(* - An error will affect the current and the following 	*)
(*   ciphertext blocks.   					*)
(*								*)
(* Further explanation can be found in Schneider 9.3 (p. 193)	*)

(* $Log: BlowfishCBC.m3,v $
 * Revision 1.1  2000/04/11 09:28:27  drt
 * Initial revision
 *
(* Revision 1.1.1.1  1999/12/14 14:58:47  drt
(* initial revision
(*
(* Revision 1.1  1999/10/18 20:55:53  drt
(* Initial revision
(* *)

IMPORT Word;
IMPORT Random;
IMPORT Blowfish;
IMPORT Text, IO, Fmt;

REVEAL T = Public BRANDED "BlowfishCBC $Revision: 1.1 $" 
 OBJECT
  feedbackL, feedbackR : Word.T;
METHODS
  encipher(a: ARRAY OF CHAR) := Encipher;
  decipher(a: ARRAY OF CHAR) := Decipher;
OVERRIDES
  init := Init;
END;    

VAR 

PROCEDURE Init( self: T; key : TEXT ) =
BEGIN
  (* Hand the key to the basic algorithm; "supercall"*)
  Blowfish.T.init( self, key ); 

  (* Set a random initialisation vector (IV) *)
  feedbackL := Random.Word();
  feedbackR := Random.Word(); 
END Init;

PROCEDURE Encipher(self: T; a: ARRAY OF CHAR) =
VAR 
  i, j: INTEGER;
  xl, xr : Word.T;

BEGIN

  FOR i:= FIRST(a) TO LAST(a) BY 8 DO
    FOR j:= 0 TO 3 DO
      IO.Put(Text.FromChar(a[i+j]));
      xl := Word.And(xl, Word.Shift(ORD(Text.FromChar(a[i+j]))),3-j);
    END;
    FOR j:= 0 TO 3 DO
      IO.Put(Text.FromChar(a[i+4+j]));
      xr := Word.And(xr, Word.Shift(ORD(Text.FromChar(a[i+4+j]))),3-j);
    END;
  IO.Put("16_" & Fmt.Unsigned(xl, 16));
  IO.Put(" 16_" & Fmt.Unsigned(xr, 16) & "\n");
  END;

END Encipher;

PROCEDURE Decipher(self: T; a: ARRAY OF CHAR) =
VAR 
  i: INTEGER;

BEGIN

  FOR i:= FIRST(a) TO LAST(a) DO
    IO.Put(Text.FromChar(a[i]));
  END;

END Decipher;

BEGIN
END BlowfishCBC.
