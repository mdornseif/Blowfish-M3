INTERFACE Blowfish;

(* Blowfish by drt@ailis.de                                       *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: Blowfish.i3,v 1.1.1.2 2000/04/11 09:28:22 drt Exp $ *)

(* This implements the basic Blowfish algorythm by Bruce Schneier *)
(* For further details see http://www.counterpane.com/blowfish/   *)

(* $Log: Blowfish.i3,v $
 * Revision 1.1.1.2  2000/04/11 09:28:22  drt
 * Version with chaining
 *
(* Revision 1.1.1.1  1999/12/14 14:58:47  drt
(* initial revision
(*
(* Revision 1.2  1999/07/22 06:51:13  drt
(* Now objectoriented.
(*
 * Revision 1.1  1999/07/12 09:06:45  drt
 * Initial revision
 * *)

IMPORT Word;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS        
    init( key : TEXT ): T;
    (* Initializes the cipher with key by doing key expansion.        *)
    (* This fills the P-array and the four S-boxes. Since this takes  *)
    (* 521 iterations Init is somehow slow.                           *)
    (* Sucessive calls to init destroy previous data structures       *)

    encipher( VAR Xl, Xr: Word.T );
    (* Enciphers 64 Bit of Plaintext with the Key gotten from         *)
    (* Init(). Input are the two half blocks Xl Xr which contain      *)
    (* the ciphertext after the call.                                 *)
  
    decipher( VAR Xl, Xr: Word.T );
    (* The oposite of Encipher                                        *)
  END;
END Blowfish.
