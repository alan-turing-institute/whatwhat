type interval =
  { first : int
  ; last : int
  }

let mk first last = { first; last }

(** As of Unicode 15.0.0. Taken from
    https://github.com/jquast/wcwidth/blob/master/wcwidth/table_zero.py *)
let zero_width_chars =
  [ mk 0x00300 0x0036f
  ; (* Combining Grave Accent  ..Combining Latin Small Le *)
    mk 0x00483 0x00489
  ; (* Combining Cyrillic Titlo..Combining Cyrillic Milli *)
    mk 0x00591 0x005bd
  ; (* Hebrew Accent Etnahta   ..Hebrew Point Meteg *)
    mk 0x005bf 0x005bf
  ; (* Hebrew Point Rafe *)
    mk 0x005c1 0x005c2
  ; (* Hebrew Point Shin Dot   ..Hebrew Point Sin Dot *)
    mk 0x005c4 0x005c5
  ; (* Hebrew Mark Upper Dot   ..Hebrew Mark Lower Dot *)
    mk 0x005c7 0x005c7
  ; (* Hebrew Point Qamats Qatan *)
    mk 0x00610 0x0061a
  ; (* Arabic Sign Sallallahou ..Arabic Small Kasra *)
    mk 0x0064b 0x0065f
  ; (* Arabic Fathatan         ..Arabic Wavy Hamza Below *)
    mk 0x00670 0x00670
  ; (* Arabic Letter Superscript Alef *)
    mk 0x006d6 0x006dc
  ; (* Arabic Small High Ligatu..Arabic Small High Seen *)
    mk 0x006df 0x006e4
  ; (* Arabic Small High Rounde..Arabic Small High Madda *)
    mk 0x006e7 0x006e8
  ; (* Arabic Small High Yeh   ..Arabic Small High Noon *)
    mk 0x006ea 0x006ed
  ; (* Arabic Empty Centre Low ..Arabic Small Low Meem *)
    mk 0x00711 0x00711
  ; (* Syriac Letter Superscript Alaph *)
    mk 0x00730 0x0074a
  ; (* Syriac Pthaha Above     ..Syriac Barrekh *)
    mk 0x007a6 0x007b0
  ; (* Thaana Abafili          ..Thaana Sukun *)
    mk 0x007eb 0x007f3
  ; (* Nko Combining Short High..Nko Combining Double Dot *)
    mk 0x007fd 0x007fd
  ; (* Nko Dantayalan *)
    mk 0x00816 0x00819
  ; (* Samaritan Mark In       ..Samaritan Mark Dagesh *)
    mk 0x0081b 0x00823
  ; (* Samaritan Mark Epentheti..Samaritan Vowel Sign A *)
    mk 0x00825 0x00827
  ; (* Samaritan Vowel Sign Sho..Samaritan Vowel Sign U *)
    mk 0x00829 0x0082d
  ; (* Samaritan Vowel Sign Lon..Samaritan Mark Nequdaa *)
    mk 0x00859 0x0085b
  ; (* Mandaic Affrication Mark..Mandaic Gemination Mark *)
    mk 0x00898 0x0089f
  ; (* Arabic Small High Word A..Arabic Half Madda Over M *)
    mk 0x008ca 0x008e1
  ; (* Arabic Small High Farsi ..Arabic Small High Sign S *)
    mk 0x008e3 0x00902
  ; (* Arabic Turned Damma Belo..Devanagari Sign Anusvara *)
    mk 0x0093a 0x0093a
  ; (* Devanagari Vowel Sign Oe *)
    mk 0x0093c 0x0093c
  ; (* Devanagari Sign Nukta *)
    mk 0x00941 0x00948
  ; (* Devanagari Vowel Sign U ..Devanagari Vowel Sign Ai *)
    mk 0x0094d 0x0094d
  ; (* Devanagari Sign Virama *)
    mk 0x00951 0x00957
  ; (* Devanagari Stress Sign U..Devanagari Vowel Sign Uu *)
    mk 0x00962 0x00963
  ; (* Devanagari Vowel Sign Vo..Devanagari Vowel Sign Vo *)
    mk 0x00981 0x00981
  ; (* Bengali Sign Candrabindu *)
    mk 0x009bc 0x009bc
  ; (* Bengali Sign Nukta *)
    mk 0x009c1 0x009c4
  ; (* Bengali Vowel Sign U    ..Bengali Vowel Sign Vocal *)
    mk 0x009cd 0x009cd
  ; (* Bengali Sign Virama *)
    mk 0x009e2 0x009e3
  ; (* Bengali Vowel Sign Vocal..Bengali Vowel Sign Vocal *)
    mk 0x009fe 0x009fe
  ; (* Bengali Sandhi Mark *)
    mk 0x00a01 0x00a02
  ; (* Gurmukhi Sign Adak Bindi..Gurmukhi Sign Bindi *)
    mk 0x00a3c 0x00a3c
  ; (* Gurmukhi Sign Nukta *)
    mk 0x00a41 0x00a42
  ; (* Gurmukhi Vowel Sign U   ..Gurmukhi Vowel Sign Uu *)
    mk 0x00a47 0x00a48
  ; (* Gurmukhi Vowel Sign Ee  ..Gurmukhi Vowel Sign Ai *)
    mk 0x00a4b 0x00a4d
  ; (* Gurmukhi Vowel Sign Oo  ..Gurmukhi Sign Virama *)
    mk 0x00a51 0x00a51
  ; (* Gurmukhi Sign Udaat *)
    mk 0x00a70 0x00a71
  ; (* Gurmukhi Tippi          ..Gurmukhi Addak *)
    mk 0x00a75 0x00a75
  ; (* Gurmukhi Sign Yakash *)
    mk 0x00a81 0x00a82
  ; (* Gujarati Sign Candrabind..Gujarati Sign Anusvara *)
    mk 0x00abc 0x00abc
  ; (* Gujarati Sign Nukta *)
    mk 0x00ac1 0x00ac5
  ; (* Gujarati Vowel Sign U   ..Gujarati Vowel Sign Cand *)
    mk 0x00ac7 0x00ac8
  ; (* Gujarati Vowel Sign E   ..Gujarati Vowel Sign Ai *)
    mk 0x00acd 0x00acd
  ; (* Gujarati Sign Virama *)
    mk 0x00ae2 0x00ae3
  ; (* Gujarati Vowel Sign Voca..Gujarati Vowel Sign Voca *)
    mk 0x00afa 0x00aff
  ; (* Gujarati Sign Sukun     ..Gujarati Sign Two-circle *)
    mk 0x00b01 0x00b01
  ; (* Oriya Sign Candrabindu *)
    mk 0x00b3c 0x00b3c
  ; (* Oriya Sign Nukta *)
    mk 0x00b3f 0x00b3f
  ; (* Oriya Vowel Sign I *)
    mk 0x00b41 0x00b44
  ; (* Oriya Vowel Sign U      ..Oriya Vowel Sign Vocalic *)
    mk 0x00b4d 0x00b4d
  ; (* Oriya Sign Virama *)
    mk 0x00b55 0x00b56
  ; (* Oriya Sign Overline     ..Oriya Ai Length Mark *)
    mk 0x00b62 0x00b63
  ; (* Oriya Vowel Sign Vocalic..Oriya Vowel Sign Vocalic *)
    mk 0x00b82 0x00b82
  ; (* Tamil Sign Anusvara *)
    mk 0x00bc0 0x00bc0
  ; (* Tamil Vowel Sign Ii *)
    mk 0x00bcd 0x00bcd
  ; (* Tamil Sign Virama *)
    mk 0x00c00 0x00c00
  ; (* Telugu Sign Combining Candrabindu Above *)
    mk 0x00c04 0x00c04
  ; (* Telugu Sign Combining Anusvara Above *)
    mk 0x00c3c 0x00c3c
  ; (* Telugu Sign Nukta *)
    mk 0x00c3e 0x00c40
  ; (* Telugu Vowel Sign Aa    ..Telugu Vowel Sign Ii *)
    mk 0x00c46 0x00c48
  ; (* Telugu Vowel Sign E     ..Telugu Vowel Sign Ai *)
    mk 0x00c4a 0x00c4d
  ; (* Telugu Vowel Sign O     ..Telugu Sign Virama *)
    mk 0x00c55 0x00c56
  ; (* Telugu Length Mark      ..Telugu Ai Length Mark *)
    mk 0x00c62 0x00c63
  ; (* Telugu Vowel Sign Vocali..Telugu Vowel Sign Vocali *)
    mk 0x00c81 0x00c81
  ; (* Kannada Sign Candrabindu *)
    mk 0x00cbc 0x00cbc
  ; (* Kannada Sign Nukta *)
    mk 0x00cbf 0x00cbf
  ; (* Kannada Vowel Sign I *)
    mk 0x00cc6 0x00cc6
  ; (* Kannada Vowel Sign E *)
    mk 0x00ccc 0x00ccd
  ; (* Kannada Vowel Sign Au   ..Kannada Sign Virama *)
    mk 0x00ce2 0x00ce3
  ; (* Kannada Vowel Sign Vocal..Kannada Vowel Sign Vocal *)
    mk 0x00d00 0x00d01
  ; (* Malayalam Sign Combining..Malayalam Sign Candrabin *)
    mk 0x00d3b 0x00d3c
  ; (* Malayalam Sign Vertical ..Malayalam Sign Circular *)
    mk 0x00d41 0x00d44
  ; (* Malayalam Vowel Sign U  ..Malayalam Vowel Sign Voc *)
    mk 0x00d4d 0x00d4d
  ; (* Malayalam Sign Virama *)
    mk 0x00d62 0x00d63
  ; (* Malayalam Vowel Sign Voc..Malayalam Vowel Sign Voc *)
    mk 0x00d81 0x00d81
  ; (* Sinhala Sign Candrabindu *)
    mk 0x00dca 0x00dca
  ; (* Sinhala Sign Al-lakuna *)
    mk 0x00dd2 0x00dd4
  ; (* Sinhala Vowel Sign Ketti..Sinhala Vowel Sign Ketti *)
    mk 0x00dd6 0x00dd6
  ; (* Sinhala Vowel Sign Diga Paa-pilla *)
    mk 0x00e31 0x00e31
  ; (* Thai Character Mai Han-akat *)
    mk 0x00e34 0x00e3a
  ; (* Thai Character Sara I   ..Thai Character Phinthu *)
    mk 0x00e47 0x00e4e
  ; (* Thai Character Maitaikhu..Thai Character Yamakkan *)
    mk 0x00eb1 0x00eb1
  ; (* Lao Vowel Sign Mai Kan *)
    mk 0x00eb4 0x00ebc
  ; (* Lao Vowel Sign I        ..Lao Semivowel Sign Lo *)
    mk 0x00ec8 0x00ece
  ; (* Lao Tone Mai Ek         ..(nil) *)
    mk 0x00f18 0x00f19
  ; (* Tibetan Astrological Sig..Tibetan Astrological Sig *)
    mk 0x00f35 0x00f35
  ; (* Tibetan Mark Ngas Bzung Nyi Zla *)
    mk 0x00f37 0x00f37
  ; (* Tibetan Mark Ngas Bzung Sgor Rtags *)
    mk 0x00f39 0x00f39
  ; (* Tibetan Mark Tsa -phru *)
    mk 0x00f71 0x00f7e
  ; (* Tibetan Vowel Sign Aa   ..Tibetan Sign Rjes Su Nga *)
    mk 0x00f80 0x00f84
  ; (* Tibetan Vowel Sign Rever..Tibetan Mark Halanta *)
    mk 0x00f86 0x00f87
  ; (* Tibetan Sign Lci Rtags  ..Tibetan Sign Yang Rtags *)
    mk 0x00f8d 0x00f97
  ; (* Tibetan Subjoined Sign L..Tibetan Subjoined Letter *)
    mk 0x00f99 0x00fbc
  ; (* Tibetan Subjoined Letter..Tibetan Subjoined Letter *)
    mk 0x00fc6 0x00fc6
  ; (* Tibetan Symbol Padma Gdan *)
    mk 0x0102d 0x01030
  ; (* Myanmar Vowel Sign I    ..Myanmar Vowel Sign Uu *)
    mk 0x01032 0x01037
  ; (* Myanmar Vowel Sign Ai   ..Myanmar Sign Dot Below *)
    mk 0x01039 0x0103a
  ; (* Myanmar Sign Virama     ..Myanmar Sign Asat *)
    mk 0x0103d 0x0103e
  ; (* Myanmar Consonant Sign M..Myanmar Consonant Sign M *)
    mk 0x01058 0x01059
  ; (* Myanmar Vowel Sign Vocal..Myanmar Vowel Sign Vocal *)
    mk 0x0105e 0x01060
  ; (* Myanmar Consonant Sign M..Myanmar Consonant Sign M *)
    mk 0x01071 0x01074
  ; (* Myanmar Vowel Sign Geba ..Myanmar Vowel Sign Kayah *)
    mk 0x01082 0x01082
  ; (* Myanmar Consonant Sign Shan Medial Wa *)
    mk 0x01085 0x01086
  ; (* Myanmar Vowel Sign Shan ..Myanmar Vowel Sign Shan *)
    mk 0x0108d 0x0108d
  ; (* Myanmar Sign Shan Council Emphatic Tone *)
    mk 0x0109d 0x0109d
  ; (* Myanmar Vowel Sign Aiton Ai *)
    mk 0x0135d 0x0135f
  ; (* Ethiopic Combining Gemin..Ethiopic Combining Gemin *)
    mk 0x01712 0x01714
  ; (* Tagalog Vowel Sign I    ..Tagalog Sign Virama *)
    mk 0x01732 0x01733
  ; (* Hanunoo Vowel Sign I    ..Hanunoo Vowel Sign U *)
    mk 0x01752 0x01753
  ; (* Buhid Vowel Sign I      ..Buhid Vowel Sign U *)
    mk 0x01772 0x01773
  ; (* Tagbanwa Vowel Sign I   ..Tagbanwa Vowel Sign U *)
    mk 0x017b4 0x017b5
  ; (* Khmer Vowel Inherent Aq ..Khmer Vowel Inherent Aa *)
    mk 0x017b7 0x017bd
  ; (* Khmer Vowel Sign I      ..Khmer Vowel Sign Ua *)
    mk 0x017c6 0x017c6
  ; (* Khmer Sign Nikahit *)
    mk 0x017c9 0x017d3
  ; (* Khmer Sign Muusikatoan  ..Khmer Sign Bathamasat *)
    mk 0x017dd 0x017dd
  ; (* Khmer Sign Atthacan *)
    mk 0x0180b 0x0180d
  ; (* Mongolian Free Variation..Mongolian Free Variation *)
    mk 0x0180f 0x0180f
  ; (* Mongolian Free Variation Selector Four *)
    mk 0x01885 0x01886
  ; (* Mongolian Letter Ali Gal..Mongolian Letter Ali Gal *)
    mk 0x018a9 0x018a9
  ; (* Mongolian Letter Ali Gali Dagalga *)
    mk 0x01920 0x01922
  ; (* Limbu Vowel Sign A      ..Limbu Vowel Sign U *)
    mk 0x01927 0x01928
  ; (* Limbu Vowel Sign E      ..Limbu Vowel Sign O *)
    mk 0x01932 0x01932
  ; (* Limbu Small Letter Anusvara *)
    mk 0x01939 0x0193b
  ; (* Limbu Sign Mukphreng    ..Limbu Sign Sa-i *)
    mk 0x01a17 0x01a18
  ; (* Buginese Vowel Sign I   ..Buginese Vowel Sign U *)
    mk 0x01a1b 0x01a1b
  ; (* Buginese Vowel Sign Ae *)
    mk 0x01a56 0x01a56
  ; (* Tai Tham Consonant Sign Medial La *)
    mk 0x01a58 0x01a5e
  ; (* Tai Tham Sign Mai Kang L..Tai Tham Consonant Sign *)
    mk 0x01a60 0x01a60
  ; (* Tai Tham Sign Sakot *)
    mk 0x01a62 0x01a62
  ; (* Tai Tham Vowel Sign Mai Sat *)
    mk 0x01a65 0x01a6c
  ; (* Tai Tham Vowel Sign I   ..Tai Tham Vowel Sign Oa B *)
    mk 0x01a73 0x01a7c
  ; (* Tai Tham Vowel Sign Oa A..Tai Tham Sign Khuen-lue *)
    mk 0x01a7f 0x01a7f
  ; (* Tai Tham Combining Cryptogrammic Dot *)
    mk 0x01ab0 0x01ace
  ; (* Combining Doubled Circum..Combining Latin Small Le *)
    mk 0x01b00 0x01b03
  ; (* Balinese Sign Ulu Ricem ..Balinese Sign Surang *)
    mk 0x01b34 0x01b34
  ; (* Balinese Sign Rerekan *)
    mk 0x01b36 0x01b3a
  ; (* Balinese Vowel Sign Ulu ..Balinese Vowel Sign Ra R *)
    mk 0x01b3c 0x01b3c
  ; (* Balinese Vowel Sign La Lenga *)
    mk 0x01b42 0x01b42
  ; (* Balinese Vowel Sign Pepet *)
    mk 0x01b6b 0x01b73
  ; (* Balinese Musical Symbol ..Balinese Musical Symbol *)
    mk 0x01b80 0x01b81
  ; (* Sundanese Sign Panyecek ..Sundanese Sign Panglayar *)
    mk 0x01ba2 0x01ba5
  ; (* Sundanese Consonant Sign..Sundanese Vowel Sign Pan *)
    mk 0x01ba8 0x01ba9
  ; (* Sundanese Vowel Sign Pam..Sundanese Vowel Sign Pan *)
    mk 0x01bab 0x01bad
  ; (* Sundanese Sign Virama   ..Sundanese Consonant Sign *)
    mk 0x01be6 0x01be6
  ; (* Batak Sign Tompi *)
    mk 0x01be8 0x01be9
  ; (* Batak Vowel Sign Pakpak ..Batak Vowel Sign Ee *)
    mk 0x01bed 0x01bed
  ; (* Batak Vowel Sign Karo O *)
    mk 0x01bef 0x01bf1
  ; (* Batak Vowel Sign U For S..Batak Consonant Sign H *)
    mk 0x01c2c 0x01c33
  ; (* Lepcha Vowel Sign E     ..Lepcha Consonant Sign T *)
    mk 0x01c36 0x01c37
  ; (* Lepcha Sign Ran         ..Lepcha Sign Nukta *)
    mk 0x01cd0 0x01cd2
  ; (* Vedic Tone Karshana     ..Vedic Tone Prenkha *)
    mk 0x01cd4 0x01ce0
  ; (* Vedic Sign Yajurvedic Mi..Vedic Tone Rigvedic Kash *)
    mk 0x01ce2 0x01ce8
  ; (* Vedic Sign Visarga Svari..Vedic Sign Visarga Anuda *)
    mk 0x01ced 0x01ced
  ; (* Vedic Sign Tiryak *)
    mk 0x01cf4 0x01cf4
  ; (* Vedic Tone Candra Above *)
    mk 0x01cf8 0x01cf9
  ; (* Vedic Tone Ring Above   ..Vedic Tone Double Ring A *)
    mk 0x01dc0 0x01dff
  ; (* Combining Dotted Grave A..Combining Right Arrowhea *)
    mk 0x020d0 0x020f0
  ; (* Combining Left Harpoon A..Combining Asterisk Above *)
    mk 0x02cef 0x02cf1
  ; (* Coptic Combining Ni Abov..Coptic Combining Spiritu *)
    mk 0x02d7f 0x02d7f
  ; (* Tifinagh Consonant Joiner *)
    mk 0x02de0 0x02dff
  ; (* Combining Cyrillic Lette..Combining Cyrillic Lette *)
    mk 0x0302a 0x0302d
  ; (* Ideographic Level Tone M..Ideographic Entering Ton *)
    mk 0x03099 0x0309a
  ; (* Combining Katakana-hirag..Combining Katakana-hirag *)
    mk 0x0a66f 0x0a672
  ; (* Combining Cyrillic Vzmet..Combining Cyrillic Thous *)
    mk 0x0a674 0x0a67d
  ; (* Combining Cyrillic Lette..Combining Cyrillic Payer *)
    mk 0x0a69e 0x0a69f
  ; (* Combining Cyrillic Lette..Combining Cyrillic Lette *)
    mk 0x0a6f0 0x0a6f1
  ; (* Bamum Combining Mark Koq..Bamum Combining Mark Tuk *)
    mk 0x0a802 0x0a802
  ; (* Syloti Nagri Sign Dvisvara *)
    mk 0x0a806 0x0a806
  ; (* Syloti Nagri Sign Hasanta *)
    mk 0x0a80b 0x0a80b
  ; (* Syloti Nagri Sign Anusvara *)
    mk 0x0a825 0x0a826
  ; (* Syloti Nagri Vowel Sign ..Syloti Nagri Vowel Sign *)
    mk 0x0a82c 0x0a82c
  ; (* Syloti Nagri Sign Alternate Hasanta *)
    mk 0x0a8c4 0x0a8c5
  ; (* Saurashtra Sign Virama  ..Saurashtra Sign Candrabi *)
    mk 0x0a8e0 0x0a8f1
  ; (* Combining Devanagari Dig..Combining Devanagari Sig *)
    mk 0x0a8ff 0x0a8ff
  ; (* Devanagari Vowel Sign Ay *)
    mk 0x0a926 0x0a92d
  ; (* Kayah Li Vowel Ue       ..Kayah Li Tone Calya Plop *)
    mk 0x0a947 0x0a951
  ; (* Rejang Vowel Sign I     ..Rejang Consonant Sign R *)
    mk 0x0a980 0x0a982
  ; (* Javanese Sign Panyangga ..Javanese Sign Layar *)
    mk 0x0a9b3 0x0a9b3
  ; (* Javanese Sign Cecak Telu *)
    mk 0x0a9b6 0x0a9b9
  ; (* Javanese Vowel Sign Wulu..Javanese Vowel Sign Suku *)
    mk 0x0a9bc 0x0a9bd
  ; (* Javanese Vowel Sign Pepe..Javanese Consonant Sign *)
    mk 0x0a9e5 0x0a9e5
  ; (* Myanmar Sign Shan Saw *)
    mk 0x0aa29 0x0aa2e
  ; (* Cham Vowel Sign Aa      ..Cham Vowel Sign Oe *)
    mk 0x0aa31 0x0aa32
  ; (* Cham Vowel Sign Au      ..Cham Vowel Sign Ue *)
    mk 0x0aa35 0x0aa36
  ; (* Cham Consonant Sign La  ..Cham Consonant Sign Wa *)
    mk 0x0aa43 0x0aa43
  ; (* Cham Consonant Sign Final Ng *)
    mk 0x0aa4c 0x0aa4c
  ; (* Cham Consonant Sign Final M *)
    mk 0x0aa7c 0x0aa7c
  ; (* Myanmar Sign Tai Laing Tone-2 *)
    mk 0x0aab0 0x0aab0
  ; (* Tai Viet Mai Kang *)
    mk 0x0aab2 0x0aab4
  ; (* Tai Viet Vowel I        ..Tai Viet Vowel U *)
    mk 0x0aab7 0x0aab8
  ; (* Tai Viet Mai Khit       ..Tai Viet Vowel Ia *)
    mk 0x0aabe 0x0aabf
  ; (* Tai Viet Vowel Am       ..Tai Viet Tone Mai Ek *)
    mk 0x0aac1 0x0aac1
  ; (* Tai Viet Tone Mai Tho *)
    mk 0x0aaec 0x0aaed
  ; (* Meetei Mayek Vowel Sign ..Meetei Mayek Vowel Sign *)
    mk 0x0aaf6 0x0aaf6
  ; (* Meetei Mayek Virama *)
    mk 0x0abe5 0x0abe5
  ; (* Meetei Mayek Vowel Sign Anap *)
    mk 0x0abe8 0x0abe8
  ; (* Meetei Mayek Vowel Sign Unap *)
    mk 0x0abed 0x0abed
  ; (* Meetei Mayek Apun Iyek *)
    mk 0x0fb1e 0x0fb1e
  ; (* Hebrew Point Judeo-spanish Varika *)
    mk 0x0fe00 0x0fe0f
  ; (* Variation Selector-1    ..Variation Selector-16 *)
    mk 0x0fe20 0x0fe2f
  ; (* Combining Ligature Left ..Combining Cyrillic Titlo *)
    mk 0x101fd 0x101fd
  ; (* Phaistos Disc Sign Combining Oblique Stroke *)
    mk 0x102e0 0x102e0
  ; (* Coptic Epact Thousands Mark *)
    mk 0x10376 0x1037a
  ; (* Combining Old Permic Let..Combining Old Permic Let *)
    mk 0x10a01 0x10a03
  ; (* Kharoshthi Vowel Sign I ..Kharoshthi Vowel Sign Vo *)
    mk 0x10a05 0x10a06
  ; (* Kharoshthi Vowel Sign E ..Kharoshthi Vowel Sign O *)
    mk 0x10a0c 0x10a0f
  ; (* Kharoshthi Vowel Length ..Kharoshthi Sign Visarga *)
    mk 0x10a38 0x10a3a
  ; (* Kharoshthi Sign Bar Abov..Kharoshthi Sign Dot Belo *)
    mk 0x10a3f 0x10a3f
  ; (* Kharoshthi Virama *)
    mk 0x10ae5 0x10ae6
  ; (* Manichaean Abbreviation ..Manichaean Abbreviation *)
    mk 0x10d24 0x10d27
  ; (* Hanifi Rohingya Sign Har..Hanifi Rohingya Sign Tas *)
    mk 0x10eab 0x10eac
  ; (* Yezidi Combining Hamza M..Yezidi Combining Madda M *)
    mk 0x10efd 0x10eff
  ; (* (nil) *)
    mk 0x10f46 0x10f50
  ; (* Sogdian Combining Dot Be..Sogdian Combining Stroke *)
    mk 0x10f82 0x10f85
  ; (* Old Uyghur Combining Dot..Old Uyghur Combining Two *)
    mk 0x11001 0x11001
  ; (* Brahmi Sign Anusvara *)
    mk 0x11038 0x11046
  ; (* Brahmi Vowel Sign Aa    ..Brahmi Virama *)
    mk 0x11070 0x11070
  ; (* Brahmi Sign Old Tamil Virama *)
    mk 0x11073 0x11074
  ; (* Brahmi Vowel Sign Old Ta..Brahmi Vowel Sign Old Ta *)
    mk 0x1107f 0x11081
  ; (* Brahmi Number Joiner    ..Kaithi Sign Anusvara *)
    mk 0x110b3 0x110b6
  ; (* Kaithi Vowel Sign U     ..Kaithi Vowel Sign Ai *)
    mk 0x110b9 0x110ba
  ; (* Kaithi Sign Virama      ..Kaithi Sign Nukta *)
    mk 0x110c2 0x110c2
  ; (* Kaithi Vowel Sign Vocalic R *)
    mk 0x11100 0x11102
  ; (* Chakma Sign Candrabindu ..Chakma Sign Visarga *)
    mk 0x11127 0x1112b
  ; (* Chakma Vowel Sign A     ..Chakma Vowel Sign Uu *)
    mk 0x1112d 0x11134
  ; (* Chakma Vowel Sign Ai    ..Chakma Maayyaa *)
    mk 0x11173 0x11173
  ; (* Mahajani Sign Nukta *)
    mk 0x11180 0x11181
  ; (* Sharada Sign Candrabindu..Sharada Sign Anusvara *)
    mk 0x111b6 0x111be
  ; (* Sharada Vowel Sign U    ..Sharada Vowel Sign O *)
    mk 0x111c9 0x111cc
  ; (* Sharada Sandhi Mark     ..Sharada Extra Short Vowe *)
    mk 0x111cf 0x111cf
  ; (* Sharada Sign Inverted Candrabindu *)
    mk 0x1122f 0x11231
  ; (* Khojki Vowel Sign U     ..Khojki Vowel Sign Ai *)
    mk 0x11234 0x11234
  ; (* Khojki Sign Anusvara *)
    mk 0x11236 0x11237
  ; (* Khojki Sign Nukta       ..Khojki Sign Shadda *)
    mk 0x1123e 0x1123e
  ; (* Khojki Sign Sukun *)
    mk 0x11241 0x11241
  ; (* (nil) *)
    mk 0x112df 0x112df
  ; (* Khudawadi Sign Anusvara *)
    mk 0x112e3 0x112ea
  ; (* Khudawadi Vowel Sign U  ..Khudawadi Sign Virama *)
    mk 0x11300 0x11301
  ; (* Grantha Sign Combining A..Grantha Sign Candrabindu *)
    mk 0x1133b 0x1133c
  ; (* Combining Bindu Below   ..Grantha Sign Nukta *)
    mk 0x11340 0x11340
  ; (* Grantha Vowel Sign Ii *)
    mk 0x11366 0x1136c
  ; (* Combining Grantha Digit ..Combining Grantha Digit *)
    mk 0x11370 0x11374
  ; (* Combining Grantha Letter..Combining Grantha Letter *)
    mk 0x11438 0x1143f
  ; (* Newa Vowel Sign U       ..Newa Vowel Sign Ai *)
    mk 0x11442 0x11444
  ; (* Newa Sign Virama        ..Newa Sign Anusvara *)
    mk 0x11446 0x11446
  ; (* Newa Sign Nukta *)
    mk 0x1145e 0x1145e
  ; (* Newa Sandhi Mark *)
    mk 0x114b3 0x114b8
  ; (* Tirhuta Vowel Sign U    ..Tirhuta Vowel Sign Vocal *)
    mk 0x114ba 0x114ba
  ; (* Tirhuta Vowel Sign Short E *)
    mk 0x114bf 0x114c0
  ; (* Tirhuta Sign Candrabindu..Tirhuta Sign Anusvara *)
    mk 0x114c2 0x114c3
  ; (* Tirhuta Sign Virama     ..Tirhuta Sign Nukta *)
    mk 0x115b2 0x115b5
  ; (* Siddham Vowel Sign U    ..Siddham Vowel Sign Vocal *)
    mk 0x115bc 0x115bd
  ; (* Siddham Sign Candrabindu..Siddham Sign Anusvara *)
    mk 0x115bf 0x115c0
  ; (* Siddham Sign Virama     ..Siddham Sign Nukta *)
    mk 0x115dc 0x115dd
  ; (* Siddham Vowel Sign Alter..Siddham Vowel Sign Alter *)
    mk 0x11633 0x1163a
  ; (* Modi Vowel Sign U       ..Modi Vowel Sign Ai *)
    mk 0x1163d 0x1163d
  ; (* Modi Sign Anusvara *)
    mk 0x1163f 0x11640
  ; (* Modi Sign Virama        ..Modi Sign Ardhacandra *)
    mk 0x116ab 0x116ab
  ; (* Takri Sign Anusvara *)
    mk 0x116ad 0x116ad
  ; (* Takri Vowel Sign Aa *)
    mk 0x116b0 0x116b5
  ; (* Takri Vowel Sign U      ..Takri Vowel Sign Au *)
    mk 0x116b7 0x116b7
  ; (* Takri Sign Nukta *)
    mk 0x1171d 0x1171f
  ; (* Ahom Consonant Sign Medi..Ahom Consonant Sign Medi *)
    mk 0x11722 0x11725
  ; (* Ahom Vowel Sign I       ..Ahom Vowel Sign Uu *)
    mk 0x11727 0x1172b
  ; (* Ahom Vowel Sign Aw      ..Ahom Sign Killer *)
    mk 0x1182f 0x11837
  ; (* Dogra Vowel Sign U      ..Dogra Sign Anusvara *)
    mk 0x11839 0x1183a
  ; (* Dogra Sign Virama       ..Dogra Sign Nukta *)
    mk 0x1193b 0x1193c
  ; (* Dives Akuru Sign Anusvar..Dives Akuru Sign Candrab *)
    mk 0x1193e 0x1193e
  ; (* Dives Akuru Virama *)
    mk 0x11943 0x11943
  ; (* Dives Akuru Sign Nukta *)
    mk 0x119d4 0x119d7
  ; (* Nandinagari Vowel Sign U..Nandinagari Vowel Sign V *)
    mk 0x119da 0x119db
  ; (* Nandinagari Vowel Sign E..Nandinagari Vowel Sign A *)
    mk 0x119e0 0x119e0
  ; (* Nandinagari Sign Virama *)
    mk 0x11a01 0x11a0a
  ; (* Zanabazar Square Vowel S..Zanabazar Square Vowel L *)
    mk 0x11a33 0x11a38
  ; (* Zanabazar Square Final C..Zanabazar Square Sign An *)
    mk 0x11a3b 0x11a3e
  ; (* Zanabazar Square Cluster..Zanabazar Square Cluster *)
    mk 0x11a47 0x11a47
  ; (* Zanabazar Square Subjoiner *)
    mk 0x11a51 0x11a56
  ; (* Soyombo Vowel Sign I    ..Soyombo Vowel Sign Oe *)
    mk 0x11a59 0x11a5b
  ; (* Soyombo Vowel Sign Vocal..Soyombo Vowel Length Mar *)
    mk 0x11a8a 0x11a96
  ; (* Soyombo Final Consonant ..Soyombo Sign Anusvara *)
    mk 0x11a98 0x11a99
  ; (* Soyombo Gemination Mark ..Soyombo Subjoiner *)
    mk 0x11c30 0x11c36
  ; (* Bhaiksuki Vowel Sign I  ..Bhaiksuki Vowel Sign Voc *)
    mk 0x11c38 0x11c3d
  ; (* Bhaiksuki Vowel Sign E  ..Bhaiksuki Sign Anusvara *)
    mk 0x11c3f 0x11c3f
  ; (* Bhaiksuki Sign Virama *)
    mk 0x11c92 0x11ca7
  ; (* Marchen Subjoined Letter..Marchen Subjoined Letter *)
    mk 0x11caa 0x11cb0
  ; (* Marchen Subjoined Letter..Marchen Vowel Sign Aa *)
    mk 0x11cb2 0x11cb3
  ; (* Marchen Vowel Sign U    ..Marchen Vowel Sign E *)
    mk 0x11cb5 0x11cb6
  ; (* Marchen Sign Anusvara   ..Marchen Sign Candrabindu *)
    mk 0x11d31 0x11d36
  ; (* Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign *)
    mk 0x11d3a 0x11d3a
  ; (* Masaram Gondi Vowel Sign E *)
    mk 0x11d3c 0x11d3d
  ; (* Masaram Gondi Vowel Sign..Masaram Gondi Vowel Sign *)
    mk 0x11d3f 0x11d45
  ; (* Masaram Gondi Vowel Sign..Masaram Gondi Virama *)
    mk 0x11d47 0x11d47
  ; (* Masaram Gondi Ra-kara *)
    mk 0x11d90 0x11d91
  ; (* Gunjala Gondi Vowel Sign..Gunjala Gondi Vowel Sign *)
    mk 0x11d95 0x11d95
  ; (* Gunjala Gondi Sign Anusvara *)
    mk 0x11d97 0x11d97
  ; (* Gunjala Gondi Virama *)
    mk 0x11ef3 0x11ef4
  ; (* Makasar Vowel Sign I    ..Makasar Vowel Sign U *)
    mk 0x11f00 0x11f01
  ; (* (nil) *)
    mk 0x11f36 0x11f3a
  ; (* (nil) *)
    mk 0x11f40 0x11f40
  ; (* (nil) *)
    mk 0x11f42 0x11f42
  ; (* (nil) *)
    mk 0x13440 0x13440
  ; (* (nil) *)
    mk 0x13447 0x13455
  ; (* (nil) *)
    mk 0x16af0 0x16af4
  ; (* Bassa Vah Combining High..Bassa Vah Combining High *)
    mk 0x16b30 0x16b36
  ; (* Pahawh Hmong Mark Cim Tu..Pahawh Hmong Mark Cim Ta *)
    mk 0x16f4f 0x16f4f
  ; (* Miao Sign Consonant Modifier Bar *)
    mk 0x16f8f 0x16f92
  ; (* Miao Tone Right         ..Miao Tone Below *)
    mk 0x16fe4 0x16fe4
  ; (* Khitan Small Script Filler *)
    mk 0x1bc9d 0x1bc9e
  ; (* Duployan Thick Letter Se..Duployan Double Mark *)
    mk 0x1cf00 0x1cf2d
  ; (* Znamenny Combining Mark ..Znamenny Combining Mark *)
    mk 0x1cf30 0x1cf46
  ; (* Znamenny Combining Tonal..Znamenny Priznak Modifie *)
    mk 0x1d167 0x1d169
  ; (* Musical Symbol Combining..Musical Symbol Combining *)
    mk 0x1d17b 0x1d182
  ; (* Musical Symbol Combining..Musical Symbol Combining *)
    mk 0x1d185 0x1d18b
  ; (* Musical Symbol Combining..Musical Symbol Combining *)
    mk 0x1d1aa 0x1d1ad
  ; (* Musical Symbol Combining..Musical Symbol Combining *)
    mk 0x1d242 0x1d244
  ; (* Combining Greek Musical ..Combining Greek Musical *)
    mk 0x1da00 0x1da36
  ; (* Signwriting Head Rim    ..Signwriting Air Sucking *)
    mk 0x1da3b 0x1da6c
  ; (* Signwriting Mouth Closed..Signwriting Excitement *)
    mk 0x1da75 0x1da75
  ; (* Signwriting Upper Body Tilting From Hip Joints *)
    mk 0x1da84 0x1da84
  ; (* Signwriting Location Head Neck *)
    mk 0x1da9b 0x1da9f
  ; (* Signwriting Fill Modifie..Signwriting Fill Modifie *)
    mk 0x1daa1 0x1daaf
  ; (* Signwriting Rotation Mod..Signwriting Rotation Mod *)
    mk 0x1e000 0x1e006
  ; (* Combining Glagolitic Let..Combining Glagolitic Let *)
    mk 0x1e008 0x1e018
  ; (* Combining Glagolitic Let..Combining Glagolitic Let *)
    mk 0x1e01b 0x1e021
  ; (* Combining Glagolitic Let..Combining Glagolitic Let *)
    mk 0x1e023 0x1e024
  ; (* Combining Glagolitic Let..Combining Glagolitic Let *)
    mk 0x1e026 0x1e02a
  ; (* Combining Glagolitic Let..Combining Glagolitic Let *)
    mk 0x1e08f 0x1e08f
  ; (* (nil) *)
    mk 0x1e130 0x1e136
  ; (* Nyiakeng Puachue Hmong T..Nyiakeng Puachue Hmong T *)
    mk 0x1e2ae 0x1e2ae
  ; (* Toto Sign Rising Tone *)
    mk 0x1e2ec 0x1e2ef
  ; (* Wancho Tone Tup         ..Wancho Tone Koini *)
    mk 0x1e4ec 0x1e4ef
  ; (* (nil) *)
    mk 0x1e8d0 0x1e8d6
  ; (* Mende Kikakui Combining ..Mende Kikakui Combining *)
    mk 0x1e944 0x1e94a
  ; (* Adlam Alif Lengthener   ..Adlam Nukta *)
    mk 0xe0100 0xe01ef (* Variation Selector-17   ..Variation Selector-256 *)
  ]
;;

(** As of Unicode 15.0.0. Taken from
    https://github.com/jquast/wcwidth/blob/master/wcwidth/table_wide.py *)
let wide_chars =
  [ mk 0x01100 0x0115f
  ; (* Hangul Choseong Kiyeok  ..Hangul Choseong Filler *)
    mk 0x0231a 0x0231b
  ; (* Watch                   ..Hourglass *)
    mk 0x02329 0x0232a
  ; (* Left-pointing Angle Brac..Right-pointing Angle Bra *)
    mk 0x023e9 0x023ec
  ; (* Black Right-pointing Dou..Black Down-pointing Doub *)
    mk 0x023f0 0x023f0
  ; (* Alarm Clock *)
    mk 0x023f3 0x023f3
  ; (* Hourglass With Flowing Sand *)
    mk 0x025fd 0x025fe
  ; (* White Medium Small Squar..Black Medium Small Squar *)
    mk 0x02614 0x02615
  ; (* Umbrella With Rain Drops..Hot Beverage *)
    mk 0x02648 0x02653
  ; (* Aries                   ..Pisces *)
    mk 0x0267f 0x0267f
  ; (* Wheelchair Symbol *)
    mk 0x02693 0x02693
  ; (* Anchor *)
    mk 0x026a1 0x026a1
  ; (* High Voltage Sign *)
    mk 0x026aa 0x026ab
  ; (* Medium White Circle     ..Medium Black Circle *)
    mk 0x026bd 0x026be
  ; (* Soccer Ball             ..Baseball *)
    mk 0x026c4 0x026c5
  ; (* Snowman Without Snow    ..Sun Behind Cloud *)
    mk 0x026ce 0x026ce
  ; (* Ophiuchus *)
    mk 0x026d4 0x026d4
  ; (* No Entry *)
    mk 0x026ea 0x026ea
  ; (* Church *)
    mk 0x026f2 0x026f3
  ; (* Fountain                ..Flag In Hole *)
    mk 0x026f5 0x026f5
  ; (* Sailboat *)
    mk 0x026fa 0x026fa
  ; (* Tent *)
    mk 0x026fd 0x026fd
  ; (* Fuel Pump *)
    mk 0x02705 0x02705
  ; (* White Heavy Check Mark *)
    mk 0x0270a 0x0270b
  ; (* Raised Fist             ..Raised Hand *)
    mk 0x02728 0x02728
  ; (* Sparkles *)
    mk 0x0274c 0x0274c
  ; (* Cross Mark *)
    mk 0x0274e 0x0274e
  ; (* Negative Squared Cross Mark *)
    mk 0x02753 0x02755
  ; (* Black Question Mark Orna..White Exclamation Mark O *)
    mk 0x02757 0x02757
  ; (* Heavy Exclamation Mark Symbol *)
    mk 0x02795 0x02797
  ; (* Heavy Plus Sign         ..Heavy Division Sign *)
    mk 0x027b0 0x027b0
  ; (* Curly Loop *)
    mk 0x027bf 0x027bf
  ; (* Double Curly Loop *)
    mk 0x02b1b 0x02b1c
  ; (* Black Large Square      ..White Large Square *)
    mk 0x02b50 0x02b50
  ; (* White Medium Star *)
    mk 0x02b55 0x02b55
  ; (* Heavy Large Circle *)
    mk 0x02e80 0x02e99
  ; (* Cjk Radical Repeat      ..Cjk Radical Rap *)
    mk 0x02e9b 0x02ef3
  ; (* Cjk Radical Choke       ..Cjk Radical C-simplified *)
    mk 0x02f00 0x02fd5
  ; (* Kangxi Radical One      ..Kangxi Radical Flute *)
    mk 0x02ff0 0x02ffb
  ; (* Ideographic Description ..Ideographic Description *)
    mk 0x03000 0x0303e
  ; (* Ideographic Space       ..Ideographic Variation In *)
    mk 0x03041 0x03096
  ; (* Hiragana Letter Small A ..Hiragana Letter Small Ke *)
    mk 0x03099 0x030ff
  ; (* Combining Katakana-hirag..Katakana Digraph Koto *)
    mk 0x03105 0x0312f
  ; (* Bopomofo Letter B       ..Bopomofo Letter Nn *)
    mk 0x03131 0x0318e
  ; (* Hangul Letter Kiyeok    ..Hangul Letter Araeae *)
    mk 0x03190 0x031e3
  ; (* Ideographic Annotation L..Cjk Stroke Q *)
    mk 0x031f0 0x0321e
  ; (* Katakana Letter Small Ku..Parenthesized Korean Cha *)
    mk 0x03220 0x03247
  ; (* Parenthesized Ideograph ..Circled Ideograph Koto *)
    mk 0x03250 0x04dbf
  ; (* Partnership Sign        ..Cjk Unified Ideograph-4d *)
    mk 0x04e00 0x0a48c
  ; (* Cjk Unified Ideograph-4e..Yi Syllable Yyr *)
    mk 0x0a490 0x0a4c6
  ; (* Yi Radical Qot          ..Yi Radical Ke *)
    mk 0x0a960 0x0a97c
  ; (* Hangul Choseong Tikeut-m..Hangul Choseong Ssangyeo *)
    mk 0x0ac00 0x0d7a3
  ; (* Hangul Syllable Ga      ..Hangul Syllable Hih *)
    mk 0x0f900 0x0faff
  ; (* Cjk Compatibility Ideogr..(nil) *)
    mk 0x0fe10 0x0fe19
  ; (* Presentation Form For Ve..Presentation Form For Ve *)
    mk 0x0fe30 0x0fe52
  ; (* Presentation Form For Ve..Small Full Stop *)
    mk 0x0fe54 0x0fe66
  ; (* Small Semicolon         ..Small Equals Sign *)
    mk 0x0fe68 0x0fe6b
  ; (* Small Reverse Solidus   ..Small Commercial At *)
    mk 0x0ff01 0x0ff60
  ; (* Fullwidth Exclamation Ma..Fullwidth Right White Pa *)
    mk 0x0ffe0 0x0ffe6
  ; (* Fullwidth Cent Sign     ..Fullwidth Won Sign *)
    mk 0x16fe0 0x16fe4
  ; (* Tangut Iteration Mark   ..Khitan Small Script Fill *)
    mk 0x16ff0 0x16ff1
  ; (* Vietnamese Alternate Rea..Vietnamese Alternate Rea *)
    mk 0x17000 0x187f7
  ; (* (nil) *)
    mk 0x18800 0x18cd5
  ; (* Tangut Component-001    ..Khitan Small Script Char *)
    mk 0x18d00 0x18d08
  ; (* (nil) *)
    mk 0x1aff0 0x1aff3
  ; (* Katakana Letter Minnan T..Katakana Letter Minnan T *)
    mk 0x1aff5 0x1affb
  ; (* Katakana Letter Minnan T..Katakana Letter Minnan N *)
    mk 0x1affd 0x1affe
  ; (* Katakana Letter Minnan N..Katakana Letter Minnan N *)
    mk 0x1b000 0x1b122
  ; (* Katakana Letter Archaic ..Katakana Letter Archaic *)
    mk 0x1b132 0x1b132
  ; (* (nil) *)
    mk 0x1b150 0x1b152
  ; (* Hiragana Letter Small Wi..Hiragana Letter Small Wo *)
    mk 0x1b155 0x1b155
  ; (* (nil) *)
    mk 0x1b164 0x1b167
  ; (* Katakana Letter Small Wi..Katakana Letter Small N *)
    mk 0x1b170 0x1b2fb
  ; (* Nushu Character-1b170   ..Nushu Character-1b2fb *)
    mk 0x1f004 0x1f004
  ; (* Mahjong Tile Red Dragon *)
    mk 0x1f0cf 0x1f0cf
  ; (* Playing Card Black Joker *)
    mk 0x1f18e 0x1f18e
  ; (* Negative Squared Ab *)
    mk 0x1f191 0x1f19a
  ; (* Squared Cl              ..Squared Vs *)
    mk 0x1f200 0x1f202
  ; (* Square Hiragana Hoka    ..Squared Katakana Sa *)
    mk 0x1f210 0x1f23b
  ; (* Squared Cjk Unified Ideo..Squared Cjk Unified Ideo *)
    mk 0x1f240 0x1f248
  ; (* Tortoise Shell Bracketed..Tortoise Shell Bracketed *)
    mk 0x1f250 0x1f251
  ; (* Circled Ideograph Advant..Circled Ideograph Accept *)
    mk 0x1f260 0x1f265
  ; (* Rounded Symbol For Fu   ..Rounded Symbol For Cai *)
    mk 0x1f300 0x1f320
  ; (* Cyclone                 ..Shooting Star *)
    mk 0x1f32d 0x1f335
  ; (* Hot Dog                 ..Cactus *)
    mk 0x1f337 0x1f37c
  ; (* Tulip                   ..Baby Bottle *)
    mk 0x1f37e 0x1f393
  ; (* Bottle With Popping Cork..Graduation Cap *)
    mk 0x1f3a0 0x1f3ca
  ; (* Carousel Horse          ..Swimmer *)
    mk 0x1f3cf 0x1f3d3
  ; (* Cricket Bat And Ball    ..Table Tennis Paddle And *)
    mk 0x1f3e0 0x1f3f0
  ; (* House Building          ..European Castle *)
    mk 0x1f3f4 0x1f3f4
  ; (* Waving Black Flag *)
    mk 0x1f3f8 0x1f43e
  ; (* Badminton Racquet And Sh..Paw Prints *)
    mk 0x1f440 0x1f440
  ; (* Eyes *)
    mk 0x1f442 0x1f4fc
  ; (* Ear                     ..Videocassette *)
    mk 0x1f4ff 0x1f53d
  ; (* Prayer Beads            ..Down-pointing Small Red *)
    mk 0x1f54b 0x1f54e
  ; (* Kaaba                   ..Menorah With Nine Branch *)
    mk 0x1f550 0x1f567
  ; (* Clock Face One Oclock   ..Clock Face Twelve-thirty *)
    mk 0x1f57a 0x1f57a
  ; (* Man Dancing *)
    mk 0x1f595 0x1f596
  ; (* Reversed Hand With Middl..Raised Hand With Part Be *)
    mk 0x1f5a4 0x1f5a4
  ; (* Black Heart *)
    mk 0x1f5fb 0x1f64f
  ; (* Mount Fuji              ..Person With Folded Hands *)
    mk 0x1f680 0x1f6c5
  ; (* Rocket                  ..Left Luggage *)
    mk 0x1f6cc 0x1f6cc
  ; (* Sleeping Accommodation *)
    mk 0x1f6d0 0x1f6d2
  ; (* Place Of Worship        ..Shopping Trolley *)
    mk 0x1f6d5 0x1f6d7
  ; (* Hindu Temple            ..Elevator *)
    mk 0x1f6dc 0x1f6df
  ; (* (nil)                   ..Ring Buoy *)
    mk 0x1f6eb 0x1f6ec
  ; (* Airplane Departure      ..Airplane Arriving *)
    mk 0x1f6f4 0x1f6fc
  ; (* Scooter                 ..Roller Skate *)
    mk 0x1f7e0 0x1f7eb
  ; (* Large Orange Circle     ..Large Brown Square *)
    mk 0x1f7f0 0x1f7f0
  ; (* Heavy Equals Sign *)
    mk 0x1f90c 0x1f93a
  ; (* Pinched Fingers         ..Fencer *)
    mk 0x1f93c 0x1f945
  ; (* Wrestlers               ..Goal Net *)
    mk 0x1f947 0x1f9ff
  ; (* First Place Medal       ..Nazar Amulet *)
    mk 0x1fa70 0x1fa7c
  ; (* Ballet Shoes            ..Crutch *)
    mk 0x1fa80 0x1fa88
  ; (* Yo-yo                   ..(nil) *)
    mk 0x1fa90 0x1fabd
  ; (* Ringed Planet           ..(nil) *)
    mk 0x1fabf 0x1fac5
  ; (* (nil)                   ..Person With Crown *)
    mk 0x1face 0x1fadb
  ; (* (nil) *)
    mk 0x1fae0 0x1fae8
  ; (* Melting Face            ..(nil) *)
    mk 0x1faf0 0x1faf8
  ; (* Hand With Index Finger A..(nil) *)
    mk 0x20000 0x2fffd
  ; (* Cjk Unified Ideograph-20..(nil) *)
    mk 0x30000 0x3fffd (* Cjk Unified Ideograph-30..(nil) *)
  ]
;;

(** Returns true if the number [i] is inside any of the intervals in the given
    [table]. The input [table] must be sorted. *)
let binary_search (table : interval list) (i : int) : bool =
  let min, max = 0, List.length table - 1 in
  if i < (List.hd table).first || i > (List.nth table max).last
  then false
  else (
    let rec inner_search min max =
      if max < min
      then false
      else (
        let mid = (min + max) / 2 in
        if i > (List.nth table mid).last
        then inner_search (mid + 1) max
        else if i < (List.nth table mid).first
        then inner_search min (mid - 1)
        else true)
    in
    inner_search min max)
;;

let zero_width_others =
  [ 0
  ; (* Null (Cc) *)
    0x034F
  ; (* Combining grapheme joiner (Mn) *)
    0x200B
  ; (* Zero width space *)
    0x200C
  ; (* Zero width non-joiner *)
    0x200D
  ; (* Zero width joiner *)
    0x200E
  ; (* Left-to-right mark *)
    0x200F
  ; (* Right-to-left mark *)
    0x2028
  ; (* Line separator (Zl) *)
    0x2029
  ; (* Paragraph separator (Zp) *)
    0x202A
  ; (* Left-to-right embedding *)
    0x202B
  ; (* Right-to-left embedding *)
    0x202C
  ; (* Pop directional formatting *)
    0x202D
  ; (* Left-to-right override *)
    0x202E
  ; (* Right-to-left override *)
    0x2060
  ; (* Word joiner *)
    0x2061
  ; (* Function application *)
    0x2062
  ; (* Invisible times *)
    0x2063 (* Invisible separator *)
  ]
;;

let wcwidth (c : Uchar.t) =
  match Uchar.to_int c with
  | 0 -> 0
  | i when i < 32 || (i >= 0x7f && i < 0x80) -> -1
  | i when List.mem i zero_width_others -> 0
  | i when binary_search zero_width_chars i -> 0
  | i when binary_search wide_chars i -> 2
  | _ -> 1
;;

let to_reversed_utf8 (s : string) : Uchar.t list =
  (* Number of bytes *)
  let len = String.length s in
  let rec aux n accum =
    if n >= len
    then accum
    else (
      let decode = String.get_utf_8_uchar s n in
      let uc = Uchar.utf_decode_uchar decode in
      let sz = Uchar.utf_decode_length decode in
      aux (n + sz) (uc :: accum))
  in
  aux 0 []
;;

let to_utf8 (s : string) : Uchar.t list = List.rev (to_reversed_utf8 s)

let wcswidth (s : string) =
  (* We can use the reversed sequence of characters here because it's faster,
     and their order doesn't really matter once we sum them up. *)
  let uchars = to_reversed_utf8 s in
  List.fold_right
    (fun uc acc ->
      let w = wcwidth uc in
      if w = -1 || acc = -1 then -1 else acc + w)
    uchars
    0
;;
