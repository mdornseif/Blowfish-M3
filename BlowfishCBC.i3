INTERFACE BlowfishCBC;

(* Blowfish by drt@ailis.de                                       *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: BlowfishCBC.i3,v 1.1 2000/04/11 09:28:27 drt Exp $ *)

(* This implements the Blowfish algorythm by Bruce Schneier in    *)
(* CBC Mode. See Applied Cryptography 9.3 (p. 193) for further    *)
(* Details.                                                       *)

(* $Log: BlowfishCBC.i3,v $
 * Revision 1.1  2000/04/11 09:28:27  drt
 * Initial revision
 *
(* Revision 1.1.1.1  1999/12/14 14:58:47  drt
(* initial revision
(*
(* Revision 1.1  1999/12/14 14:50:27  drt
(* initial revision
(* *)

IMPORT Blowfish;

TYPE
  T <: Public;
  Public = Blowfish.T OBJECT
  METHODS
    init( key : TEXT ): T;
    (* Initializes the cipher with key by doing key expansion.        *)
    (* This fills the P-array and the four S-boxes. Since this takes  *)
    (* 521 iterations Init is somehow slow.                           *)
    (* Sucessive calls to init destroy previous data structures       *)

    encipher(a: ARRAY OF CHAR);
    (* Enciphers 64 Bit of Plaintext with the Key gotten from         *)
    (* Init(). Input are the two half blocks Xl Xr which contain      *)
    (* the ciphertext after the call.                                 *)

    decipher(a: ARRAY OF CHAR);
    (* The oposite of Encipher                                        *)
  END;
  
END BlowfishCBC.
