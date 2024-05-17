(** Zero-width characters as of Unicode 15.1.0.
    Taken from the Python wcwidth package:
    https://pypi.org/project/wcwidth/ *)
let zero_width_chars =
  [| 0x00000, 0x00000
   ; 0x000ad, 0x000ad
   ; 0x00300, 0x0036f
   ; 0x00483, 0x00489
   ; 0x00591, 0x005bd
   ; 0x005bf, 0x005bf
   ; 0x005c1, 0x005c2
   ; 0x005c4, 0x005c5
   ; 0x005c7, 0x005c7
   ; 0x00600, 0x00605
   ; 0x00610, 0x0061a
   ; 0x0061c, 0x0061c
   ; 0x0064b, 0x0065f
   ; 0x00670, 0x00670
   ; 0x006d6, 0x006dd
   ; 0x006df, 0x006e4
   ; 0x006e7, 0x006e8
   ; 0x006ea, 0x006ed
   ; 0x0070f, 0x0070f
   ; 0x00711, 0x00711
   ; 0x00730, 0x0074a
   ; 0x007a6, 0x007b0
   ; 0x007eb, 0x007f3
   ; 0x007fd, 0x007fd
   ; 0x00816, 0x00819
   ; 0x0081b, 0x00823
   ; 0x00825, 0x00827
   ; 0x00829, 0x0082d
   ; 0x00859, 0x0085b
   ; 0x00890, 0x00891
   ; 0x00898, 0x0089f
   ; 0x008ca, 0x00903
   ; 0x0093a, 0x0093c
   ; 0x0093e, 0x0094f
   ; 0x00951, 0x00957
   ; 0x00962, 0x00963
   ; 0x00981, 0x00983
   ; 0x009bc, 0x009bc
   ; 0x009be, 0x009c4
   ; 0x009c7, 0x009c8
   ; 0x009cb, 0x009cd
   ; 0x009d7, 0x009d7
   ; 0x009e2, 0x009e3
   ; 0x009fe, 0x009fe
   ; 0x00a01, 0x00a03
   ; 0x00a3c, 0x00a3c
   ; 0x00a3e, 0x00a42
   ; 0x00a47, 0x00a48
   ; 0x00a4b, 0x00a4d
   ; 0x00a51, 0x00a51
   ; 0x00a70, 0x00a71
   ; 0x00a75, 0x00a75
   ; 0x00a81, 0x00a83
   ; 0x00abc, 0x00abc
   ; 0x00abe, 0x00ac5
   ; 0x00ac7, 0x00ac9
   ; 0x00acb, 0x00acd
   ; 0x00ae2, 0x00ae3
   ; 0x00afa, 0x00aff
   ; 0x00b01, 0x00b03
   ; 0x00b3c, 0x00b3c
   ; 0x00b3e, 0x00b44
   ; 0x00b47, 0x00b48
   ; 0x00b4b, 0x00b4d
   ; 0x00b55, 0x00b57
   ; 0x00b62, 0x00b63
   ; 0x00b82, 0x00b82
   ; 0x00bbe, 0x00bc2
   ; 0x00bc6, 0x00bc8
   ; 0x00bca, 0x00bcd
   ; 0x00bd7, 0x00bd7
   ; 0x00c00, 0x00c04
   ; 0x00c3c, 0x00c3c
   ; 0x00c3e, 0x00c44
   ; 0x00c46, 0x00c48
   ; 0x00c4a, 0x00c4d
   ; 0x00c55, 0x00c56
   ; 0x00c62, 0x00c63
   ; 0x00c81, 0x00c83
   ; 0x00cbc, 0x00cbc
   ; 0x00cbe, 0x00cc4
   ; 0x00cc6, 0x00cc8
   ; 0x00cca, 0x00ccd
   ; 0x00cd5, 0x00cd6
   ; 0x00ce2, 0x00ce3
   ; 0x00cf3, 0x00cf3
   ; 0x00d00, 0x00d03
   ; 0x00d3b, 0x00d3c
   ; 0x00d3e, 0x00d44
   ; 0x00d46, 0x00d48
   ; 0x00d4a, 0x00d4d
   ; 0x00d57, 0x00d57
   ; 0x00d62, 0x00d63
   ; 0x00d81, 0x00d83
   ; 0x00dca, 0x00dca
   ; 0x00dcf, 0x00dd4
   ; 0x00dd6, 0x00dd6
   ; 0x00dd8, 0x00ddf
   ; 0x00df2, 0x00df3
   ; 0x00e31, 0x00e31
   ; 0x00e34, 0x00e3a
   ; 0x00e47, 0x00e4e
   ; 0x00eb1, 0x00eb1
   ; 0x00eb4, 0x00ebc
   ; 0x00ec8, 0x00ece
   ; 0x00f18, 0x00f19
   ; 0x00f35, 0x00f35
   ; 0x00f37, 0x00f37
   ; 0x00f39, 0x00f39
   ; 0x00f3e, 0x00f3f
   ; 0x00f71, 0x00f84
   ; 0x00f86, 0x00f87
   ; 0x00f8d, 0x00f97
   ; 0x00f99, 0x00fbc
   ; 0x00fc6, 0x00fc6
   ; 0x0102b, 0x0103e
   ; 0x01056, 0x01059
   ; 0x0105e, 0x01060
   ; 0x01062, 0x01064
   ; 0x01067, 0x0106d
   ; 0x01071, 0x01074
   ; 0x01082, 0x0108d
   ; 0x0108f, 0x0108f
   ; 0x0109a, 0x0109d
   ; 0x01160, 0x011ff
   ; 0x0135d, 0x0135f
   ; 0x01712, 0x01715
   ; 0x01732, 0x01734
   ; 0x01752, 0x01753
   ; 0x01772, 0x01773
   ; 0x017b4, 0x017d3
   ; 0x017dd, 0x017dd
   ; 0x0180b, 0x0180f
   ; 0x01885, 0x01886
   ; 0x018a9, 0x018a9
   ; 0x01920, 0x0192b
   ; 0x01930, 0x0193b
   ; 0x01a17, 0x01a1b
   ; 0x01a55, 0x01a5e
   ; 0x01a60, 0x01a7c
   ; 0x01a7f, 0x01a7f
   ; 0x01ab0, 0x01ace
   ; 0x01b00, 0x01b04
   ; 0x01b34, 0x01b44
   ; 0x01b6b, 0x01b73
   ; 0x01b80, 0x01b82
   ; 0x01ba1, 0x01bad
   ; 0x01be6, 0x01bf3
   ; 0x01c24, 0x01c37
   ; 0x01cd0, 0x01cd2
   ; 0x01cd4, 0x01ce8
   ; 0x01ced, 0x01ced
   ; 0x01cf4, 0x01cf4
   ; 0x01cf7, 0x01cf9
   ; 0x01dc0, 0x01dff
   ; 0x0200b, 0x0200f
   ; 0x02028, 0x0202e
   ; 0x02060, 0x02064
   ; 0x02066, 0x0206f
   ; 0x020d0, 0x020f0
   ; 0x02cef, 0x02cf1
   ; 0x02d7f, 0x02d7f
   ; 0x02de0, 0x02dff
   ; 0x0302a, 0x0302f
   ; 0x03099, 0x0309a
   ; 0x0a66f, 0x0a672
   ; 0x0a674, 0x0a67d
   ; 0x0a69e, 0x0a69f
   ; 0x0a6f0, 0x0a6f1
   ; 0x0a802, 0x0a802
   ; 0x0a806, 0x0a806
   ; 0x0a80b, 0x0a80b
   ; 0x0a823, 0x0a827
   ; 0x0a82c, 0x0a82c
   ; 0x0a880, 0x0a881
   ; 0x0a8b4, 0x0a8c5
   ; 0x0a8e0, 0x0a8f1
   ; 0x0a8ff, 0x0a8ff
   ; 0x0a926, 0x0a92d
   ; 0x0a947, 0x0a953
   ; 0x0a980, 0x0a983
   ; 0x0a9b3, 0x0a9c0
   ; 0x0a9e5, 0x0a9e5
   ; 0x0aa29, 0x0aa36
   ; 0x0aa43, 0x0aa43
   ; 0x0aa4c, 0x0aa4d
   ; 0x0aa7b, 0x0aa7d
   ; 0x0aab0, 0x0aab0
   ; 0x0aab2, 0x0aab4
   ; 0x0aab7, 0x0aab8
   ; 0x0aabe, 0x0aabf
   ; 0x0aac1, 0x0aac1
   ; 0x0aaeb, 0x0aaef
   ; 0x0aaf5, 0x0aaf6
   ; 0x0abe3, 0x0abea
   ; 0x0abec, 0x0abed
   ; 0x0d7b0, 0x0d7ff
   ; 0x0fb1e, 0x0fb1e
   ; 0x0fe00, 0x0fe0f
   ; 0x0fe20, 0x0fe2f
   ; 0x0feff, 0x0feff
   ; 0x0fff9, 0x0fffb
   ; 0x101fd, 0x101fd
   ; 0x102e0, 0x102e0
   ; 0x10376, 0x1037a
   ; 0x10a01, 0x10a03
   ; 0x10a05, 0x10a06
   ; 0x10a0c, 0x10a0f
   ; 0x10a38, 0x10a3a
   ; 0x10a3f, 0x10a3f
   ; 0x10ae5, 0x10ae6
   ; 0x10d24, 0x10d27
   ; 0x10eab, 0x10eac
   ; 0x10efd, 0x10eff
   ; 0x10f46, 0x10f50
   ; 0x10f82, 0x10f85
   ; 0x11000, 0x11002
   ; 0x11038, 0x11046
   ; 0x11070, 0x11070
   ; 0x11073, 0x11074
   ; 0x1107f, 0x11082
   ; 0x110b0, 0x110ba
   ; 0x110bd, 0x110bd
   ; 0x110c2, 0x110c2
   ; 0x110cd, 0x110cd
   ; 0x11100, 0x11102
   ; 0x11127, 0x11134
   ; 0x11145, 0x11146
   ; 0x11173, 0x11173
   ; 0x11180, 0x11182
   ; 0x111b3, 0x111c0
   ; 0x111c9, 0x111cc
   ; 0x111ce, 0x111cf
   ; 0x1122c, 0x11237
   ; 0x1123e, 0x1123e
   ; 0x11241, 0x11241
   ; 0x112df, 0x112ea
   ; 0x11300, 0x11303
   ; 0x1133b, 0x1133c
   ; 0x1133e, 0x11344
   ; 0x11347, 0x11348
   ; 0x1134b, 0x1134d
   ; 0x11357, 0x11357
   ; 0x11362, 0x11363
   ; 0x11366, 0x1136c
   ; 0x11370, 0x11374
   ; 0x11435, 0x11446
   ; 0x1145e, 0x1145e
   ; 0x114b0, 0x114c3
   ; 0x115af, 0x115b5
   ; 0x115b8, 0x115c0
   ; 0x115dc, 0x115dd
   ; 0x11630, 0x11640
   ; 0x116ab, 0x116b7
   ; 0x1171d, 0x1172b
   ; 0x1182c, 0x1183a
   ; 0x11930, 0x11935
   ; 0x11937, 0x11938
   ; 0x1193b, 0x1193e
   ; 0x11940, 0x11940
   ; 0x11942, 0x11943
   ; 0x119d1, 0x119d7
   ; 0x119da, 0x119e0
   ; 0x119e4, 0x119e4
   ; 0x11a01, 0x11a0a
   ; 0x11a33, 0x11a39
   ; 0x11a3b, 0x11a3e
   ; 0x11a47, 0x11a47
   ; 0x11a51, 0x11a5b
   ; 0x11a8a, 0x11a99
   ; 0x11c2f, 0x11c36
   ; 0x11c38, 0x11c3f
   ; 0x11c92, 0x11ca7
   ; 0x11ca9, 0x11cb6
   ; 0x11d31, 0x11d36
   ; 0x11d3a, 0x11d3a
   ; 0x11d3c, 0x11d3d
   ; 0x11d3f, 0x11d45
   ; 0x11d47, 0x11d47
   ; 0x11d8a, 0x11d8e
   ; 0x11d90, 0x11d91
   ; 0x11d93, 0x11d97
   ; 0x11ef3, 0x11ef6
   ; 0x11f00, 0x11f01
   ; 0x11f03, 0x11f03
   ; 0x11f34, 0x11f3a
   ; 0x11f3e, 0x11f42
   ; 0x13430, 0x13440
   ; 0x13447, 0x13455
   ; 0x16af0, 0x16af4
   ; 0x16b30, 0x16b36
   ; 0x16f4f, 0x16f4f
   ; 0x16f51, 0x16f87
   ; 0x16f8f, 0x16f92
   ; 0x16fe4, 0x16fe4
   ; 0x16ff0, 0x16ff1
   ; 0x1bc9d, 0x1bc9e
   ; 0x1bca0, 0x1bca3
   ; 0x1cf00, 0x1cf2d
   ; 0x1cf30, 0x1cf46
   ; 0x1d165, 0x1d169
   ; 0x1d16d, 0x1d182
   ; 0x1d185, 0x1d18b
   ; 0x1d1aa, 0x1d1ad
   ; 0x1d242, 0x1d244
   ; 0x1da00, 0x1da36
   ; 0x1da3b, 0x1da6c
   ; 0x1da75, 0x1da75
   ; 0x1da84, 0x1da84
   ; 0x1da9b, 0x1da9f
   ; 0x1daa1, 0x1daaf
   ; 0x1e000, 0x1e006
   ; 0x1e008, 0x1e018
   ; 0x1e01b, 0x1e021
   ; 0x1e023, 0x1e024
   ; 0x1e026, 0x1e02a
   ; 0x1e08f, 0x1e08f
   ; 0x1e130, 0x1e136
   ; 0x1e2ae, 0x1e2ae
   ; 0x1e2ec, 0x1e2ef
   ; 0x1e4ec, 0x1e4ef
   ; 0x1e8d0, 0x1e8d6
   ; 0x1e944, 0x1e94a
   ; 0x1f3fb, 0x1f3ff
   ; 0xe0001, 0xe0001
   ; 0xe0020, 0xe007f
   ; 0xe0100, 0xe01ef
  |]
;;

(** Wide characters as of Unicode 15.1.0.
    Taken from the Python wcwidth package:
    https://pypi.org/project/wcwidth/ *)
let wide_chars =
  [| 0x01100, 0x0115f
   ; 0x0231a, 0x0231b
   ; 0x02329, 0x0232a
   ; 0x023e9, 0x023ec
   ; 0x023f0, 0x023f0
   ; 0x023f3, 0x023f3
   ; 0x025fd, 0x025fe
   ; 0x02614, 0x02615
   ; 0x02648, 0x02653
   ; 0x0267f, 0x0267f
   ; 0x02693, 0x02693
   ; 0x026a1, 0x026a1
   ; 0x026aa, 0x026ab
   ; 0x026bd, 0x026be
   ; 0x026c4, 0x026c5
   ; 0x026ce, 0x026ce
   ; 0x026d4, 0x026d4
   ; 0x026ea, 0x026ea
   ; 0x026f2, 0x026f3
   ; 0x026f5, 0x026f5
   ; 0x026fa, 0x026fa
   ; 0x026fd, 0x026fd
   ; 0x02705, 0x02705
   ; 0x0270a, 0x0270b
   ; 0x02728, 0x02728
   ; 0x0274c, 0x0274c
   ; 0x0274e, 0x0274e
   ; 0x02753, 0x02755
   ; 0x02757, 0x02757
   ; 0x02795, 0x02797
   ; 0x027b0, 0x027b0
   ; 0x027bf, 0x027bf
   ; 0x02b1b, 0x02b1c
   ; 0x02b50, 0x02b50
   ; 0x02b55, 0x02b55
   ; 0x02e80, 0x02e99
   ; 0x02e9b, 0x02ef3
   ; 0x02f00, 0x02fd5
   ; 0x02ff0, 0x03029
   ; 0x03030, 0x0303e
   ; 0x03041, 0x03096
   ; 0x0309b, 0x030ff
   ; 0x03105, 0x0312f
   ; 0x03131, 0x0318e
   ; 0x03190, 0x031e3
   ; 0x031ef, 0x0321e
   ; 0x03220, 0x03247
   ; 0x03250, 0x04dbf
   ; 0x04e00, 0x0a48c
   ; 0x0a490, 0x0a4c6
   ; 0x0a960, 0x0a97c
   ; 0x0ac00, 0x0d7a3
   ; 0x0f900, 0x0faff
   ; 0x0fe10, 0x0fe19
   ; 0x0fe30, 0x0fe52
   ; 0x0fe54, 0x0fe66
   ; 0x0fe68, 0x0fe6b
   ; 0x0ff01, 0x0ff60
   ; 0x0ffe0, 0x0ffe6
   ; 0x16fe0, 0x16fe3
   ; 0x17000, 0x187f7
   ; 0x18800, 0x18cd5
   ; 0x18d00, 0x18d08
   ; 0x1aff0, 0x1aff3
   ; 0x1aff5, 0x1affb
   ; 0x1affd, 0x1affe
   ; 0x1b000, 0x1b122
   ; 0x1b132, 0x1b132
   ; 0x1b150, 0x1b152
   ; 0x1b155, 0x1b155
   ; 0x1b164, 0x1b167
   ; 0x1b170, 0x1b2fb
   ; 0x1f004, 0x1f004
   ; 0x1f0cf, 0x1f0cf
   ; 0x1f18e, 0x1f18e
   ; 0x1f191, 0x1f19a
   ; 0x1f200, 0x1f202
   ; 0x1f210, 0x1f23b
   ; 0x1f240, 0x1f248
   ; 0x1f250, 0x1f251
   ; 0x1f260, 0x1f265
   ; 0x1f300, 0x1f320
   ; 0x1f32d, 0x1f335
   ; 0x1f337, 0x1f37c
   ; 0x1f37e, 0x1f393
   ; 0x1f3a0, 0x1f3ca
   ; 0x1f3cf, 0x1f3d3
   ; 0x1f3e0, 0x1f3f0
   ; 0x1f3f4, 0x1f3f4
   ; 0x1f3f8, 0x1f3fa
   ; 0x1f400, 0x1f43e
   ; 0x1f440, 0x1f440
   ; 0x1f442, 0x1f4fc
   ; 0x1f4ff, 0x1f53d
   ; 0x1f54b, 0x1f54e
   ; 0x1f550, 0x1f567
   ; 0x1f57a, 0x1f57a
   ; 0x1f595, 0x1f596
   ; 0x1f5a4, 0x1f5a4
   ; 0x1f5fb, 0x1f64f
   ; 0x1f680, 0x1f6c5
   ; 0x1f6cc, 0x1f6cc
   ; 0x1f6d0, 0x1f6d2
   ; 0x1f6d5, 0x1f6d7
   ; 0x1f6dc, 0x1f6df
   ; 0x1f6eb, 0x1f6ec
   ; 0x1f6f4, 0x1f6fc
   ; 0x1f7e0, 0x1f7eb
   ; 0x1f7f0, 0x1f7f0
   ; 0x1f90c, 0x1f93a
   ; 0x1f93c, 0x1f945
   ; 0x1f947, 0x1f9ff
   ; 0x1fa70, 0x1fa7c
   ; 0x1fa80, 0x1fa88
   ; 0x1fa90, 0x1fabd
   ; 0x1fabf, 0x1fac5
   ; 0x1face, 0x1fadb
   ; 0x1fae0, 0x1fae8
   ; 0x1faf0, 0x1faf8
   ; 0x20000, 0x2fffd
   ; 0x30000, 0x3fffd
  |]
;;
