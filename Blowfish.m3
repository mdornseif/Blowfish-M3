MODULE Blowfish;

(* Blowfish by drt@ailis.de					  *)
(*             (K) 1999 all rights reversed                       *)

(* $Id: Blowfish.m3,v 1.1 2000/04/11 08:34:48 drt Exp $ *)

(* This implements the basic Blowfish algorythm by Bruce Schneier *)
(* For further details see http://www.counterpane.com/blowfish/   *)

(* $Log: Blowfish.m3,v $
 * Revision 1.1  2000/04/11 08:34:48  drt
 * Initial revision
 *
 * Revision 1.1  1999/07/12 09:07:15  drt
 * Initial revision
 * *)

IMPORT Word;
IMPORT Text, IO;

TYPE sbox = ARRAY [0..255] OF Word.T;
     
CONST 
  N =              16;

VAR  
  
  (* The subkeys are calculated using the Blowfish algorithm.  The          *)
  (* exact method is as follows:                                            *)
  (*                                                                        *)
  (*          1.  Initialize first the P-array and then the four S-boxes,   *)
  (*          in order, with a fixed string.  This string consists of the   *)
  (*          hexadecimal digits of pi (less the initial 3).  For example:  *)
  (*                                                                        *)
  (*                  P1 = 0x243f6a88                                       *)
  (*                  P2 = 0x85a308d3                                       *)
  (*                  P3 = 0x13198a2e                                       *)
  (*                  P4 = 0x03707344                                       *)
  
  
  P := ARRAY [0..17] OF Word.T {
  16_243f6a88, 16_85a308d3, 16_13198a2e, 16_03707344,
  16_a4093822, 16_299f31d0, 16_082efa98, 16_ec4e6c89,
  16_452821e6, 16_38d01377, 16_be5466cf, 16_34e90c6c,
  16_c0ac29b7, 16_c97c50dd, 16_3f84d5b5, 16_b5470917,
  16_9216d5d9, 16_8979fb1b};

  S  := ARRAY [0..3] OF sbox {
  sbox{16_d1310ba6, 16_98dfb5ac, 16_2ffd72db, 16_d01adfb7, 
       16_b8e1afed, 16_6a267e96, 16_ba7c9045, 16_f12c7f99, 
       16_24a19947, 16_b3916cf7, 16_0801f2e2, 16_858efc16, 
       16_636920d8, 16_71574e69, 16_a458fea3, 16_f4933d7e, 
       16_0d95748f, 16_728eb658, 16_718bcd58, 16_82154aee, 
       16_7b54a41d, 16_c25a59b5, 16_9c30d539, 16_2af26013, 
       16_c5d1b023, 16_286085f0, 16_ca417918, 16_b8db38ef, 
       16_8e79dcb0, 16_603a180e, 16_6c9e0e8b, 16_b01e8a3e, 
       16_d71577c1, 16_bd314b27, 16_78af2fda, 16_55605c60, 
       16_e65525f3, 16_aa55ab94, 16_57489862, 16_63e81440, 
       16_55ca396a, 16_2aab10b6, 16_b4cc5c34, 16_1141e8ce, 
       16_a15486af, 16_7c72e993, 16_b3ee1411, 16_636fbc2a, 
       16_2ba9c55d, 16_741831f6, 16_ce5c3e16, 16_9b87931e, 
       16_afd6ba33, 16_6c24cf5c, 16_7a325381, 16_28958677, 
       16_3b8f4898, 16_6b4bb9af, 16_c4bfe81b, 16_66282193, 
       16_61d809cc, 16_fb21a991, 16_487cac60, 16_5dec8032, 
       16_ef845d5d, 16_e98575b1, 16_dc262302, 16_eb651b88, 
       16_23893e81, 16_d396acc5, 16_0f6d6ff3, 16_83f44239, 
       16_2e0b4482, 16_a4842004, 16_69c8f04a, 16_9e1f9b5e, 
       16_21c66842, 16_f6e96c9a, 16_670c9c61, 16_abd388f0, 
       16_6a51a0d2, 16_d8542f68, 16_960fa728, 16_ab5133a3, 
       16_6eef0b6c, 16_137a3be4, 16_ba3bf050, 16_7efb2a98, 
       16_a1f1651d, 16_39af0176, 16_66ca593e, 16_82430e88, 
       16_8cee8619, 16_456f9fb4, 16_7d84a5c3, 16_3b8b5ebe, 
       16_e06f75d8, 16_85c12073, 16_401a449f, 16_56c16aa6, 
       16_4ed3aa62, 16_363f7706, 16_1bfedf72, 16_429b023d, 
       16_37d0d724, 16_d00a1248, 16_db0fead3, 16_49f1c09b, 
       16_075372c9, 16_80991b7b, 16_25d479d8, 16_f6e8def7, 
       16_e3fe501a, 16_b6794c3b, 16_976ce0bd, 16_04c006ba, 
       16_c1a94fb6, 16_409f60c4, 16_5e5c9ec2, 16_196a2463, 
       16_68fb6faf, 16_3e6c53b5, 16_1339b2eb, 16_3b52ec6f, 
       16_6dfc511f, 16_9b30952c, 16_cc814544, 16_af5ebd09, 
       16_bee3d004, 16_de334afd, 16_660f2807, 16_192e4bb3, 
       16_c0cba857, 16_45c8740f, 16_d20b5f39, 16_b9d3fbdb, 
       16_5579c0bd, 16_1a60320a, 16_d6a100c6, 16_402c7279, 
       16_679f25fe, 16_fb1fa3cc, 16_8ea5e9f8, 16_db3222f8, 
       16_3c7516df, 16_fd616b15, 16_2f501ec8, 16_ad0552ab, 
       16_323db5fa, 16_fd238760, 16_53317b48, 16_3e00df82, 
       16_9e5c57bb, 16_ca6f8ca0, 16_1a87562e, 16_df1769db, 
       16_d542a8f6, 16_287effc3, 16_ac6732c6, 16_8c4f5573, 
       16_695b27b0, 16_bbca58c8, 16_e1ffa35d, 16_b8f011a0, 
       16_10fa3d98, 16_fd2183b8, 16_4afcb56c, 16_2dd1d35b, 
       16_9a53e479, 16_b6f84565, 16_d28e49bc, 16_4bfb9790, 
       16_e1ddf2da, 16_a4cb7e33, 16_62fb1341, 16_cee4c6e8, 
       16_ef20cada, 16_36774c01, 16_d07e9efe, 16_2bf11fb4, 
       16_95dbda4d, 16_ae909198, 16_eaad8e71, 16_6b93d5a0, 
       16_d08ed1d0, 16_afc725e0, 16_8e3c5b2f, 16_8e7594b7, 
       16_8ff6e2fb, 16_f2122b64, 16_8888b812, 16_900df01c, 
       16_4fad5ea0, 16_688fc31c, 16_d1cff191, 16_b3a8c1ad, 
       16_2f2f2218, 16_be0e1777, 16_ea752dfe, 16_8b021fa1, 
       16_e5a0cc0f, 16_b56f74e8, 16_18acf3d6, 16_ce89e299, 
       16_b4a84fe0, 16_fd13e0b7, 16_7cc43b81, 16_d2ada8d9, 
       16_165fa266, 16_80957705, 16_93cc7314, 16_211a1477, 
       16_e6ad2065, 16_77b5fa86, 16_c75442f5, 16_fb9d35cf, 
       16_ebcdaf0c, 16_7b3e89a0, 16_d6411bd3, 16_ae1e7e49, 
       16_00250e2d, 16_2071b35e, 16_226800bb, 16_57b8e0af, 
       16_2464369b, 16_f009b91e, 16_5563911d, 16_59dfa6aa, 
       16_78c14389, 16_d95a537f, 16_207d5ba2, 16_02e5b9c5, 
       16_83260376, 16_6295cfa9, 16_11c81968, 16_4e734a41, 
       16_b3472dca, 16_7b14a94a, 16_1b510052, 16_9a532915, 
       16_d60f573f, 16_bc9bc6e4, 16_2b60a476, 16_81e67400, 
       16_08ba6fb5, 16_571be91f, 16_f296ec6b, 16_2a0dd915, 
       16_b6636521, 16_e7b9f9b6, 16_ff34052e, 16_c5855664,
       16_53b02d5d, 16_a99f8fa1, 16_08ba4799, 16_6e85076a}, 
  sbox{16_4b7a70e9, 16_b5b32944, 16_db75092e, 16_c4192623, 
       16_ad6ea6b0, 16_49a7df7d, 16_9cee60b8, 16_8fedb266, 
       16_ecaa8c71, 16_699a17ff, 16_5664526c, 16_c2b19ee1, 
       16_193602a5, 16_75094c29, 16_a0591340, 16_e4183a3e, 
       16_3f54989a, 16_5b429d65, 16_6b8fe4d6, 16_99f73fd6, 
       16_a1d29c07, 16_efe830f5, 16_4d2d38e6, 16_f0255dc1, 
       16_4cdd2086, 16_8470eb26, 16_6382e9c6, 16_021ecc5e, 
       16_09686b3f, 16_3ebaefc9, 16_3c971814, 16_6b6a70a1, 
       16_687f3584, 16_52a0e286, 16_b79c5305, 16_aa500737, 
       16_3e07841c, 16_7fdeae5c, 16_8e7d44ec, 16_5716f2b8, 
       16_b03ada37, 16_f0500c0d, 16_f01c1f04, 16_0200b3ff, 
       16_ae0cf51a, 16_3cb574b2, 16_25837a58, 16_dc0921bd, 
       16_d19113f9, 16_7ca92ff6, 16_94324773, 16_22f54701, 
       16_3ae5e581, 16_37c2dadc, 16_c8b57634, 16_9af3dda7, 
       16_a9446146, 16_0fd0030e, 16_ecc8c73e, 16_a4751e41, 
       16_e238cd99, 16_3bea0e2f, 16_3280bba1, 16_183eb331, 
       16_4e548b38, 16_4f6db908, 16_6f420d03, 16_f60a04bf, 
       16_2cb81290, 16_24977c79, 16_5679b072, 16_bcaf89af, 
       16_de9a771f, 16_d9930810, 16_b38bae12, 16_dccf3f2e, 
       16_5512721f, 16_2e6b7124, 16_501adde6, 16_9f84cd87, 
       16_7a584718, 16_7408da17, 16_bc9f9abc, 16_e94b7d8c, 
       16_ec7aec3a, 16_db851dfa, 16_63094366, 16_c464c3d2, 
       16_ef1c1847, 16_3215d908, 16_dd433b37, 16_24c2ba16, 
       16_12a14d43, 16_2a65c451, 16_50940002, 16_133ae4dd, 
       16_71dff89e, 16_10314e55, 16_81ac77d6, 16_5f11199b, 
       16_043556f1, 16_d7a3c76b, 16_3c11183b, 16_5924a509, 
       16_f28fe6ed, 16_97f1fbfa, 16_9ebabf2c, 16_1e153c6e, 
       16_86e34570, 16_eae96fb1, 16_860e5e0a, 16_5a3e2ab3, 
       16_771fe71c, 16_4e3d06fa, 16_2965dcb9, 16_99e71d0f, 
       16_803e89d6, 16_5266c825, 16_2e4cc978, 16_9c10b36a, 
       16_c6150eba, 16_94e2ea78, 16_a5fc3c53, 16_1e0a2df4, 
       16_f2f74ea7, 16_361d2b3d, 16_1939260f, 16_19c27960, 
       16_5223a708, 16_f71312b6, 16_ebadfe6e, 16_eac31f66, 
       16_e3bc4595, 16_a67bc883, 16_b17f37d1, 16_018cff28, 
       16_c332ddef, 16_be6c5aa5, 16_65582185, 16_68ab9802, 
       16_eecea50f, 16_db2f953b, 16_2aef7dad, 16_5b6e2f84, 
       16_1521b628, 16_29076170, 16_ecdd4775, 16_619f1510, 
       16_13cca830, 16_eb61bd96, 16_0334fe1e, 16_aa0363cf, 
       16_b5735c90, 16_4c70a239, 16_d59e9e0b, 16_cbaade14, 
       16_eecc86bc, 16_60622ca7, 16_9cab5cab, 16_b2f3846e, 
       16_648b1eaf, 16_19bdf0ca, 16_a02369b9, 16_655abb50, 
       16_40685a32, 16_3c2ab4b3, 16_319ee9d5, 16_c021b8f7, 
       16_9b540b19, 16_875fa099, 16_95f7997e, 16_623d7da8, 
       16_f837889a, 16_97e32d77, 16_11ed935f, 16_16681281, 
       16_0e358829, 16_c7e61fd6, 16_96dedfa1, 16_7858ba99, 
       16_57f584a5, 16_1b227263, 16_9b83c3ff, 16_1ac24696, 
       16_cdb30aeb, 16_532e3054, 16_8fd948e4, 16_6dbc3128, 
       16_58ebf2ef, 16_34c6ffea, 16_fe28ed61, 16_ee7c3c73, 
       16_5d4a14d9, 16_e864b7e3, 16_42105d14, 16_203e13e0, 
       16_45eee2b6, 16_a3aaabea, 16_db6c4f15, 16_facb4fd0, 
       16_c742f442, 16_ef6abbb5, 16_654f3b1d, 16_41cd2105, 
       16_d81e799e, 16_86854dc7, 16_e44b476a, 16_3d816250, 
       16_cf62a1f2, 16_5b8d2646, 16_fc8883a0, 16_c1c7b6a3, 
       16_7f1524c3, 16_69cb7492, 16_47848a0b, 16_5692b285, 
       16_095bbf00, 16_ad19489d, 16_1462b174, 16_23820e00, 
       16_58428d2a, 16_0c55f5ea, 16_1dadf43e, 16_233f7061, 
       16_3372f092, 16_8d937e41, 16_d65fecf1, 16_6c223bdb, 
       16_7cde3759, 16_cbee7460, 16_4085f2a7, 16_ce77326e, 
       16_a6078084, 16_19f8509e, 16_e8efd855, 16_61d99735, 
       16_a969a7aa, 16_c50c06c2, 16_5a04abfc, 16_800bcadc, 
       16_9e447a2e, 16_c3453484, 16_fdd56705, 16_0e1e9ec9, 
       16_db73dbd3, 16_105588cd, 16_675fda79, 16_e3674340, 
       16_c5c43465, 16_713e38d8, 16_3d28f89e, 16_f16dff20, 
       16_153e21e7, 16_8fb03d4a, 16_e6e39f2b, 16_db83adf7}, 
  sbox{16_e93d5a68, 16_948140f7, 16_f64c261c, 16_94692934, 
       16_411520f7, 16_7602d4f7, 16_bcf46b2e, 16_d4a20068, 
       16_d4082471, 16_3320f46a, 16_43b7d4b7, 16_500061af, 
       16_1e39f62e, 16_97244546, 16_14214f74, 16_bf8b8840, 
       16_4d95fc1d, 16_96b591af, 16_70f4ddd3, 16_66a02f45, 
       16_bfbc09ec, 16_03bd9785, 16_7fac6dd0, 16_31cb8504, 
       16_96eb27b3, 16_55fd3941, 16_da2547e6, 16_abca0a9a, 
       16_28507825, 16_530429f4, 16_0a2c86da, 16_e9b66dfb, 
       16_68dc1462, 16_d7486900, 16_680ec0a4, 16_27a18dee, 
       16_4f3ffea2, 16_e887ad8c, 16_b58ce006, 16_7af4d6b6, 
       16_aace1e7c, 16_d3375fec, 16_ce78a399, 16_406b2a42, 
       16_20fe9e35, 16_d9f385b9, 16_ee39d7ab, 16_3b124e8b, 
       16_1dc9faf7, 16_4b6d1856, 16_26a36631, 16_eae397b2, 
       16_3a6efa74, 16_dd5b4332, 16_6841e7f7, 16_ca7820fb, 
       16_fb0af54e, 16_d8feb397, 16_454056ac, 16_ba489527, 
       16_55533a3a, 16_20838d87, 16_fe6ba9b7, 16_d096954b, 
       16_55a867bc, 16_a1159a58, 16_cca92963, 16_99e1db33, 
       16_a62a4a56, 16_3f3125f9, 16_5ef47e1c, 16_9029317c, 
       16_fdf8e802, 16_04272f70, 16_80bb155c, 16_05282ce3, 
       16_95c11548, 16_e4c66d22, 16_48c1133f, 16_c70f86dc, 
       16_07f9c9ee, 16_41041f0f, 16_404779a4, 16_5d886e17, 
       16_325f51eb, 16_d59bc0d1, 16_f2bcc18f, 16_41113564, 
       16_257b7834, 16_602a9c60, 16_dff8e8a3, 16_1f636c1b, 
       16_0e12b4c2, 16_02e1329e, 16_af664fd1, 16_cad18115, 
       16_6b2395e0, 16_333e92e1, 16_3b240b62, 16_eebeb922, 
       16_85b2a20e, 16_e6ba0d99, 16_de720c8c, 16_2da2f728, 
       16_d0127845, 16_95b794fd, 16_647d0862, 16_e7ccf5f0, 
       16_5449a36f, 16_877d48fa, 16_c39dfd27, 16_f33e8d1e, 
       16_0a476341, 16_992eff74, 16_3a6f6eab, 16_f4f8fd37, 
       16_a812dc60, 16_a1ebddf8, 16_991be14c, 16_db6e6b0d, 
       16_c67b5510, 16_6d672c37, 16_2765d43b, 16_dcd0e804, 
       16_f1290dc7, 16_cc00ffa3, 16_b5390f92, 16_690fed0b, 
       16_667b9ffb, 16_cedb7d9c, 16_a091cf0b, 16_d9155ea3, 
       16_bb132f88, 16_515bad24, 16_7b9479bf, 16_763bd6eb, 
       16_37392eb3, 16_cc115979, 16_8026e297, 16_f42e312d, 
       16_6842ada7, 16_c66a2b3b, 16_12754ccc, 16_782ef11c, 
       16_6a124237, 16_b79251e7, 16_06a1bbe6, 16_4bfb6350, 
       16_1a6b1018, 16_11caedfa, 16_3d25bdd8, 16_e2e1c3c9, 
       16_44421659, 16_0a121386, 16_d90cec6e, 16_d5abea2a, 
       16_64af674e, 16_da86a85f, 16_bebfe988, 16_64e4c3fe, 
       16_9dbc8057, 16_f0f7c086, 16_60787bf8, 16_6003604d, 
       16_d1fd8346, 16_f6381fb0, 16_7745ae04, 16_d736fccc, 
       16_83426b33, 16_f01eab71, 16_b0804187, 16_3c005e5f, 
       16_77a057be, 16_bde8ae24, 16_55464299, 16_bf582e61, 
       16_4e58f48f, 16_f2ddfda2, 16_f474ef38, 16_8789bdc2, 
       16_5366f9c3, 16_c8b38e74, 16_b475f255, 16_46fcd9b9, 
       16_7aeb2661, 16_8b1ddf84, 16_846a0e79, 16_915f95e2, 
       16_466e598e, 16_20b45770, 16_8cd55591, 16_c902de4c, 
       16_b90bace1, 16_bb8205d0, 16_11a86248, 16_7574a99e, 
       16_b77f19b6, 16_e0a9dc09, 16_662d09a1, 16_c4324633, 
       16_e85a1f02, 16_09f0be8c, 16_4a99a025, 16_1d6efe10, 
       16_1ab93d1d, 16_0ba5a4df, 16_a186f20f, 16_2868f169, 
       16_dcb7da83, 16_573906fe, 16_a1e2ce9b, 16_4fcd7f52, 
       16_50115e01, 16_a70683fa, 16_a002b5c4, 16_0de6d027, 
       16_9af88c27, 16_773f8641, 16_c3604c06, 16_61a806b5, 
       16_f0177a28, 16_c0f586e0, 16_006058aa, 16_30dc7d62, 
       16_11e69ed7, 16_2338ea63, 16_53c2dd94, 16_c2c21634, 
       16_bbcbee56, 16_90bcb6de, 16_ebfc7da1, 16_ce591d76, 
       16_6f05e409, 16_4b7c0188, 16_39720a3d, 16_7c927c24, 
       16_86e3725f, 16_724d9db9, 16_1ac15bb4, 16_d39eb8fc, 
       16_ed545578, 16_08fca5b5, 16_d83d7cd3, 16_4dad0fc4, 
       16_1e50ef5e, 16_b161e6f8, 16_a28514d9, 16_6c51133c, 
       16_6fd5c7e7, 16_56e14ec4, 16_362abfce, 16_ddc6c837, 
       16_d79a3234, 16_92638212, 16_670efa8e, 16_406000e0}, 
  sbox{16_3a39ce37, 16_d3faf5cf, 16_abc27737, 16_5ac52d1b, 
       16_5cb0679e, 16_4fa33742, 16_d3822740, 16_99bc9bbe, 
       16_d5118e9d, 16_bf0f7315, 16_d62d1c7e, 16_c700c47b, 
       16_b78c1b6b, 16_21a19045, 16_b26eb1be, 16_6a366eb4, 
       16_5748ab2f, 16_bc946e79, 16_c6a376d2, 16_6549c2c8, 
       16_530ff8ee, 16_468dde7d, 16_d5730a1d, 16_4cd04dc6, 
       16_2939bbdb, 16_a9ba4650, 16_ac9526e8, 16_be5ee304, 
       16_a1fad5f0, 16_6a2d519a, 16_63ef8ce2, 16_9a86ee22, 
       16_c089c2b8, 16_43242ef6, 16_a51e03aa, 16_9cf2d0a4, 
       16_83c061ba, 16_9be96a4d, 16_8fe51550, 16_ba645bd6, 
       16_2826a2f9, 16_a73a3ae1, 16_4ba99586, 16_ef5562e9, 
       16_c72fefd3, 16_f752f7da, 16_3f046f69, 16_77fa0a59, 
       16_80e4a915, 16_87b08601, 16_9b09e6ad, 16_3b3ee593, 
       16_e990fd5a, 16_9e34d797, 16_2cf0b7d9, 16_022b8b51, 
       16_96d5ac3a, 16_017da67d, 16_d1cf3ed6, 16_7c7d2d28, 
       16_1f9f25cf, 16_adf2b89b, 16_5ad6b472, 16_5a88f54c, 
       16_e029ac71, 16_e019a5e6, 16_47b0acfd, 16_ed93fa9b, 
       16_e8d3c48d, 16_283b57cc, 16_f8d56629, 16_79132e28, 
       16_785f0191, 16_ed756055, 16_f7960e44, 16_e3d35e8c, 
       16_15056dd4, 16_88f46dba, 16_03a16125, 16_0564f0bd, 
       16_c3eb9e15, 16_3c9057a2, 16_97271aec, 16_a93a072a, 
       16_1b3f6d9b, 16_1e6321f5, 16_f59c66fb, 16_26dcf319, 
       16_7533d928, 16_b155fdf5, 16_03563482, 16_8aba3cbb, 
       16_28517711, 16_c20ad9f8, 16_abcc5167, 16_ccad925f, 
       16_4de81751, 16_3830dc8e, 16_379d5862, 16_9320f991, 
       16_ea7a90c2, 16_fb3e7bce, 16_5121ce64, 16_774fbe32, 
       16_a8b6e37e, 16_c3293d46, 16_48de5369, 16_6413e680, 
       16_a2ae0810, 16_dd6db224, 16_69852dfd, 16_09072166, 
       16_b39a460a, 16_6445c0dd, 16_586cdecf, 16_1c20c8ae, 
       16_5bbef7dd, 16_1b588d40, 16_ccd2017f, 16_6bb4e3bb, 
       16_dda26a7e, 16_3a59ff45, 16_3e350a44, 16_bcb4cdd5, 
       16_72eacea8, 16_fa6484bb, 16_8d6612ae, 16_bf3c6f47, 
       16_d29be463, 16_542f5d9e, 16_aec2771b, 16_f64e6370, 
       16_740e0d8d, 16_e75b1357, 16_f8721671, 16_af537d5d, 
       16_4040cb08, 16_4eb4e2cc, 16_34d2466a, 16_0115af84, 
       16_e1b00428, 16_95983a1d, 16_06b89fb4, 16_ce6ea048, 
       16_6f3f3b82, 16_3520ab82, 16_011a1d4b, 16_277227f8, 
       16_611560b1, 16_e7933fdc, 16_bb3a792b, 16_344525bd, 
       16_a08839e1, 16_51ce794b, 16_2f32c9b7, 16_a01fbac9, 
       16_e01cc87e, 16_bcc7d1f6, 16_cf0111c3, 16_a1e8aac7, 
       16_1a908749, 16_d44fbd9a, 16_d0dadecb, 16_d50ada38, 
       16_0339c32a, 16_c6913667, 16_8df9317c, 16_e0b12b4f, 
       16_f79e59b7, 16_43f5bb3a, 16_f2d519ff, 16_27d9459c, 
       16_bf97222c, 16_15e6fc2a, 16_0f91fc71, 16_9b941525, 
       16_fae59361, 16_ceb69ceb, 16_c2a86459, 16_12baa8d1, 
       16_b6c1075e, 16_e3056a0c, 16_10d25065, 16_cb03a442, 
       16_e0ec6e0e, 16_1698db3b, 16_4c98a0be, 16_3278e964, 
       16_9f1f9532, 16_e0d392df, 16_d3a0342b, 16_8971f21e, 
       16_1b0a7441, 16_4ba3348c, 16_c5be7120, 16_c37632d8, 
       16_df359f8d, 16_9b992f2e, 16_e60b6f47, 16_0fe3f11d, 
       16_e54cda54, 16_1edad891, 16_ce6279cf, 16_cd3e7e6f, 
       16_1618b166, 16_fd2c1d05, 16_848fd2c5, 16_f6fb2299, 
       16_f523f357, 16_a6327623, 16_93a83531, 16_56cccd02, 
       16_acf08162, 16_5a75ebb5, 16_6e163697, 16_88d273cc, 
       16_de966292, 16_81b949d0, 16_4c50901b, 16_71c65614, 
       16_e6c6c7bd, 16_327a140a, 16_45e1d006, 16_c3f27b9a, 
       16_c9aa53fd, 16_62a80f00, 16_bb25bfe2, 16_35bdd2f6, 
       16_71126905, 16_b2040222, 16_b6cbcf7c, 16_cd769c2b, 
       16_53113ec0, 16_1640e3d3, 16_38abbd60, 16_2547adf0, 
       16_ba38209c, 16_f746ce76, 16_77afa1c5, 16_20756060, 
       16_85cbfe4e, 16_8ae88dd8, 16_7aaaf9b0, 16_4cf9aa7e, 
       16_1948c25c, 16_02fb8a8c, 16_01c36ae4, 16_d6ebe1f9, 
       16_90d4f869, 16_a65cdea0, 16_3f09252d, 16_c208e69f, 
       16_b74e6132, 16_ce77e25b, 16_578fdfe3, 16_3ac372e6}};

(* Initialize Blowfish *)

PROCEDURE Init( key : TEXT) =

  VAR
    i, j, k, data, datal, datar : Word.T;
    
  BEGIN
    
    (*      2.  XOR P1 with the first 32 bits of the key, XOR P2 with the  *)
    (*      second 32-bits of the key, and so on for all bits of the key   *)
    (*      (possibly up to P14).  Repeatedly cycle through the key bits   *)
    (*      until the entire P-array has been XORed with key bits.  (For   *)
    (*      every short key, there is at least one equivalent longer       *)
    (*      key; for example, if A is a 64-bit key, then AA, AAA, etc.,    *)
    (*      are equivalent keys.)                                          *)
									  
									  
    j :=  0;                                                     
    FOR i := FIRST(P) TO LAST(P) DO                                       
      data := 0;                                                             
      FOR k := 1 TO 4 DO
        data := Word.Shift(data, 8) + ORD(Text.GetChar(key, j)); 
        INC(j);
        IF j > Text.Length(key)-1 THEN
          j := 0;
        END;
      END;
      P[i] :=  Word.Xor(P[i], data);
    END;
  
    (*      3.  Encrypt the all-zero string with the Blowfish algorithm,  *)
    (*          using the subkeys described in steps (1) and (2).         *)
    (*      4.  Replace P1 and P2 with the output of step (3).            *)
    (*      5.  Encrypt the output of step (3) using the Blowfish         *)
    (*         algorithm with the modified subkeys.                       *)
    (*      6.  Replace P3 and P4 with the output of step (5).            *)
 									  
    datal := 0;
    datar := 0;
    
    FOR i := FIRST(P) TO LAST(P) BY 2 DO
      Encipher(datal, datar);
      
      P[i] := datal;
      P[i+1] := datar;
    END;


    (*      7.  Continue the process, replacing all entries of the P-     *)
    (*          array, and then all four S-boxes in order, with the       *)   
    (*          output of the continuously-changing Blowfish algorithm.   *)

    FOR i:= 0 TO 3 DO
      FOR j := FIRST(sbox) TO LAST(sbox) BY 2 DO
        Encipher(datal, datar);
        
        S[i][j] := datal;
        S[i][j + 1] := datar;
      END;
    END;
    
    (* In total, 521 iterations are required to generate all required     *)
    (* subkeys.  Applications can store the subkeys rather than execute   *)
    (* this derivation process multiple times.                            *)
  END Init;

  
PROCEDURE Encipher( VAR Xl, Xr : Word.T) =
  
  VAR
    temp, i : Word.T;
    xl, xr : Word.T;
    
  BEGIN
    xl := Xl;
    xr := Xr;

    FOR i := 0 TO N-1 DO    
      xl := Word.Xor(xl, P[i]);
      xr := Word.Xor(F(xl), xr);
      
      (* exchange xl and xr *)
      temp := xl;
      xl := xr;
      xr := temp;

    END;

    (* exchange xl and xr *)
    temp := xl;
    xl := xr;
    xr := temp;
    
    xr :=  Word.Xor(xr, P[16]);
    xl :=  Word.Xor(xl, P[17]);

    Xl := xl;
    Xr := xr;

  END Encipher;

PROCEDURE Decipher( VAR Xl, Xr : Word.T ) =
  
  VAR
    temp, i : Word.T;
    xl, xr : Word.T;

  BEGIN
    xl := Xl;
    xr := Xr;
    
    FOR i := N+1 TO 2 BY -1 DO 
      xl := Word.Xor(xl, P[i]);
      xr := Word.Xor(F(xl), xr);

      (* exchange xl and xr *)
      temp := xl;
      xl := xr;
      xr := temp;
    END;


    (* exchange xl and xr *)
    temp := xl;
    xl := xr;
    xr := temp;
            
    xr :=  Word.Xor(xr, P[1]);
    xl :=  Word.Xor(xl, P[0]);

    Xl := xl;
    Xr := xr;
    
  END Decipher;

PROCEDURE F( x : Word.T) :  Word.T =
  
  VAR
    a, b, c, d :  Word.T;
    y :  Word.T; 

  BEGIN

    d :=  Word.And(x, 16_ff);
    c :=  Word.Shift(Word.And(x, 16_ff00),-8);
    b :=  Word.Shift(Word.And(x, 16_ff0000),-16);
    a :=  Word.Shift(Word.And(x, 16_ff000000),-24);

    y := S[0][a] + S[1][b];
    y := Word.Xor(y, S[2][c]) + S[3][d];
    
    RETURN y;

  END F;
									 
BEGIN									    
END Blowfish.