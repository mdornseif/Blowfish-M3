INTERFACE Blowfish;

(* Blowfish by drt@ailis.de					  *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: Blowfish.i3,v 1.1 2000/04/11 08:34:43 drt Exp $ *)

(* This implements the basic Blowfish algorythm by Bruce Schneier *)
(* For further details see http://www.counterpane.com/blowfish/   *)

(* $Log: Blowfish.i3,v $
 * Revision 1.1  2000/04/11 08:34:43  drt
 * Initial revision
 *
 * Revision 1.1  1999/07/12 09:06:45  drt
 * Initial revision
 * *)

IMPORT Word;

PROCEDURE Init( key : TEXT );
(* Initializes the cipher with key by doing key expansion.        *)
(* This fills the P-array and the four S-boxes. Since this takes  *)
(* 521 iterations Init is somehow slow.                           *)
(* Sucessive calls to init destroy previous data structures       *)

PROCEDURE Encipher( VAR Xl, Xr : Word.T );
(* Enciphers a 64 Bit of Plaintext with the Key gotten from 	  *)
(* Init(). Input are the two half blocks Xl Xr which contain      *)
(* after the call the ciphertext.				  *)

PROCEDURE Decipher( VAR Xl, Xr : Word.T );
(* The oposite of Encipher 					  *)

END Blowfish.