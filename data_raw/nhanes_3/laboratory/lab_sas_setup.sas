    FILENAME LAB "D:\LAB\DAT\LAB.DAT" LRECL=1979;
    *** LRECL includes 2 positions for CRLF, assuming use of PC SAS;

    DATA WORK;
      INFILE LAB MISSOVER;

      LENGTH
        SEQN      4
        DMPFSEQ   5
        DMPSTAT   3
        DMARETHN  3
        DMARACER  3
        DMAETHNR  3
        HSSEX     3
        HSAGEIR   3
        HSAGEU    3
        HSAITMOR  4
        HSFSIZER  3
        HSHSIZER  3
        DMPCNTYR  3
        DMPFIPSR  3
        DMPMETRO  3
        DMPCREGN  3
        DMPPIR    8
        SDPPHASE  3
        SDPPSU6   4
        SDPSTRA6  4
        SDPPSU1   4
        SDPSTRA1  4
        SDPPSU2   4
        SDPSTRA2  4
        WTPFQX6   8
        WTPFEX6   8
        WTPFHX6   8
        WTPFALG6  8
        WTPFCNS6  8
        WTPFSD6   8
        WTPFMD6   8
        WTPFHSD6  8
        WTPFHMD6  8
        WTPFQX1   8
        WTPFEX1   8
        WTPFHX1   8
        WTPFALG1  8
        WTPFCNS1  8
        WTPFSD1   8
        WTPFMD1   8
        WTPFHSD1  8
        WTPFHMD1  8
        WTPFQX2   8
        WTPFEX2   8
        WTPFHX2   8
        WTPFALG2  8
        WTPFCNS2  8
        WTPFSD2   8
        WTPFMD2   8
        WTPFHSD2  8
        WTPFHMD2  8
        WTPQRP1   8
        WTPQRP2   8
        WTPQRP3   8
        WTPQRP4   8
        WTPQRP5   8
        WTPQRP6   8
        WTPQRP7   8
        WTPQRP8   8
        WTPQRP9   8
        WTPQRP10  8
        WTPQRP11  8
        WTPQRP12  8
        WTPQRP13  8
        WTPQRP14  8
        WTPQRP15  8
        WTPQRP16  8
        WTPQRP17  8
        WTPQRP18  8
        WTPQRP19  8
        WTPQRP20  8
        WTPQRP21  8
        WTPQRP22  8
        WTPQRP23  8
        WTPQRP24  8
        WTPQRP25  8
        WTPQRP26  8
        WTPQRP27  8
        WTPQRP28  8
        WTPQRP29  8
        WTPQRP30  8
        WTPQRP31  8
        WTPQRP32  8
        WTPQRP33  8
        WTPQRP34  8
        WTPQRP35  8
        WTPQRP36  8
        WTPQRP37  8
        WTPQRP38  8
        WTPQRP39  8
        WTPQRP40  8
        WTPQRP41  8
        WTPQRP42  8
        WTPQRP43  8
        WTPQRP44  8
        WTPQRP45  8
        WTPQRP46  8
        WTPQRP47  8
        WTPQRP48  8
        WTPQRP49  8
        WTPQRP50  8
        WTPQRP51  8
        WTPQRP52  8
        WTPXRP1   8
        WTPXRP2   8
        WTPXRP3   8
        WTPXRP4   8
        WTPXRP5   8
        WTPXRP6   8
        WTPXRP7   8
        WTPXRP8   8
        WTPXRP9   8
        WTPXRP10  8
        WTPXRP11  8
        WTPXRP12  8
        WTPXRP13  8
        WTPXRP14  8
        WTPXRP15  8
        WTPXRP16  8
        WTPXRP17  8
        WTPXRP18  8
        WTPXRP19  8
        WTPXRP20  8
        WTPXRP21  8
        WTPXRP22  8
        WTPXRP23  8
        WTPXRP24  8
        WTPXRP25  8
        WTPXRP26  8
        WTPXRP27  8
        WTPXRP28  8
        WTPXRP29  8
        WTPXRP30  8
        WTPXRP31  8
        WTPXRP32  8
        WTPXRP33  8
        WTPXRP34  8
        WTPXRP35  8
        WTPXRP36  8
        WTPXRP37  8
        WTPXRP38  8
        WTPXRP39  8
        WTPXRP40  8
        WTPXRP41  8
        WTPXRP42  8
        WTPXRP43  8
        WTPXRP44  8
        WTPXRP45  8
        WTPXRP46  8
        WTPXRP47  8
        WTPXRP48  8
        WTPXRP49  8
        WTPXRP50  8
        WTPXRP51  8
        WTPXRP52  8
        HYAITMO   3
        MXPLANG   3
        MXPSESSR  3
        MXPTIDW   3
        MXPAXTMR  4
        HXPTIDW   3
        HXPAXTMR  4
        HXPSESSR  3
        PHPLANG   3
        PHPHEMO   3
        PHPCHM2   3
        PHPINSU   3
        PHPSNTI  $5
        PHPSNDA   3
        PHPDRIN   3
        PHPDRTI  $5
        PHPDRDA   3
        PHPFAST   8
        PHPBEST  $5
        WCP       8
        WCPSI     8
        LMPPCNT   8
        MOPPCNT   8
        GRPPCNT   8
        LMP       8
        MOP       8
        GRP       8
        RCP       8
        RCPSI     8
        HGP       8
        HGPSI     8
        HTP       8
        HTPSI     8
        MVPSI     8
        MCPSI     8
        MHP       8
        MHPSI     8
        RWP       8
        RWPSI     8
        PLP       8
        PLPSI     8
        DWP       8
        PVPSI     8
        GRPDIF    3
        LMPDIF    3
        MOPDIF    3
        EOP       3
        BOP       3
        BLP       3
        PRP       3
        MEP       3
        MLP       3
        BAP       3
        LAP       3
        ANP       3
        BSP       3
        HZP       3
        PKP       3
        POP       3
        MRP       3
        MIP       3
        SIP       3
        SHP       3
        TTP       3
        TXP       3
        VUP       3
        PBP       8
        PBPSI     8
        EPP       4
        EPPSI     8
        FEP       3
        FEPSI     8
        TIP       4
        TIPSI     8
        PXP       8
        FRP       4
        FRPSI     4
        FOP       8
        FOPSI     8
        RBP       4
        RBPSI     8
        VBP       4
        VBPSI     8
        VCP       8
        VCPSI     8
        ICPSI     8
        CAPSI     8
        SEP       4
        SEPSI     8
        VAP       3
        VAPSI     8
        VEP       4
        VEPSI     8
        ACP       3
        ACPSI     8
        BCP       4
        BCPSI     8
        BXP       3
        BXPSI     8
        LUP       3
        LUPSI     8
        LYP       3
        LYPSI     8
        REP       3
        REPSI     8
        COP       8
        TCP       3
        TCPSI     8
        TGP       4
        TGPSI     8
        LCP       3
        LCPSI     8
        HDP       3
        HDPSI     8
        AAP       8
        AAPSI     8
        ABP       3
        ABPSI     8
        LPP       3
        LPPSI     8
        FHPSI     8
        LHPSI     8
        FBP       4
        FBPSI     8
        CRP       8
        TEP       8
        AHP       3
        HBP       3
        SSP       3
        SAP       3
        HCP       3
        DHP       3
        H1P       3
        H2P       3
        RUP       8
        RUPUNIT   4
        VRP       8
        TOP       3
        RFP       4
        L1P       8
        HPP       3
        NAPSI     8
        SKPSI     8
        CLPSI     8
        C3PSI     3
        SCP       8
        SCPSI     8
        PSP       8
        PSPSI     8
        UAP       8
        UAPSI     8
        SGP       3
        SGPSI     8
        BUP       3
        BUPSI     8
        TBP       8
        TBPSI     8
        CEP       8
        CEPSI     8
        SFP       3
        SFPSI     8
        CHP       4
        CHPSI     8
        TRP       4
        TRPSI     8
        ASPSI     3
        ATPSI     3
        GGPSI     4
        LDPSI     4
        APPSI     4
        TPP       8
        TPPSI     3
        AMP       8
        AMPSI     3
        GBP       8
        GBPSI     3
        OSPSI     3
        GHP       8
        GHPMETH   3
        G1P       8
        G1PSI     8
        G1PCODE   3
        G1PTIM1   3
        G1PTIM2   3
        G2P       8
        G2PSI     8
        C1P       8
        C1PSI     8
        C2P       8
        C2PSI     8
        I1P       8
        I1PSI     8
        I1P2PFLG  3
        I2P       8
        I2PSI     8
        UDP       8
        UDPSI     8
        URP       8
        URPSI     8
        UBP       8
        UIP       8
      ;

      FORMAT
        DMPPIR   Z6.3
        WTPFQX6  Z9.2
        WTPFEX6  Z9.2
        WTPFHX6  Z9.2
        WTPFALG6 Z9.2
        WTPFCNS6 Z9.2
        WTPFSD6  Z9.2
        WTPFMD6  Z9.2
        WTPFHSD6 Z9.2
        WTPFHMD6 Z9.2
        WTPFQX1  Z9.2
        WTPFEX1  Z9.2
        WTPFHX1  Z9.2
        WTPFALG1 Z9.2
        WTPFCNS1 Z9.2
        WTPFSD1  Z9.2
        WTPFMD1  Z9.2
        WTPFHSD1 Z9.2
        WTPFHMD1 Z9.2
        WTPFQX2  Z9.2
        WTPFEX2  Z9.2
        WTPFHX2  Z9.2
        WTPFALG2 Z9.2
        WTPFCNS2 Z9.2
        WTPFSD2  Z9.2
        WTPFMD2  Z9.2
        WTPFHSD2 Z9.2
        WTPFHMD2 Z9.2
        WTPQRP1  Z9.2
        WTPQRP2  Z9.2
        WTPQRP3  Z9.2
        WTPQRP4  Z9.2
        WTPQRP5  Z9.2
        WTPQRP6  Z9.2
        WTPQRP7  Z9.2
        WTPQRP8  Z9.2
        WTPQRP9  Z9.2
        WTPQRP10 Z9.2
        WTPQRP11 Z9.2
        WTPQRP12 Z9.2
        WTPQRP13 Z9.2
        WTPQRP14 Z9.2
        WTPQRP15 Z9.2
        WTPQRP16 Z9.2
        WTPQRP17 Z9.2
        WTPQRP18 Z9.2
        WTPQRP19 Z9.2
        WTPQRP20 Z9.2
        WTPQRP21 Z9.2
        WTPQRP22 Z9.2
        WTPQRP23 Z9.2
        WTPQRP24 Z9.2
        WTPQRP25 Z9.2
        WTPQRP26 Z9.2
        WTPQRP27 Z9.2
        WTPQRP28 Z9.2
        WTPQRP29 Z9.2
        WTPQRP30 Z9.2
        WTPQRP31 Z9.2
        WTPQRP32 Z9.2
        WTPQRP33 Z9.2
        WTPQRP34 Z9.2
        WTPQRP35 Z9.2
        WTPQRP36 Z9.2
        WTPQRP37 Z9.2
        WTPQRP38 Z9.2
        WTPQRP39 Z9.2
        WTPQRP40 Z9.2
        WTPQRP41 Z9.2
        WTPQRP42 Z9.2
        WTPQRP43 Z9.2
        WTPQRP44 Z9.2
        WTPQRP45 Z9.2
        WTPQRP46 Z9.2
        WTPQRP47 Z9.2
        WTPQRP48 Z9.2
        WTPQRP49 Z9.2
        WTPQRP50 Z9.2
        WTPQRP51 Z9.2
        WTPQRP52 Z9.2
        WTPXRP1  Z9.2
        WTPXRP2  Z9.2
        WTPXRP3  Z9.2
        WTPXRP4  Z9.2
        WTPXRP5  Z9.2
        WTPXRP6  Z9.2
        WTPXRP7  Z9.2
        WTPXRP8  Z9.2
        WTPXRP9  Z9.2
        WTPXRP10 Z9.2
        WTPXRP11 Z9.2
        WTPXRP12 Z9.2
        WTPXRP13 Z9.2
        WTPXRP14 Z9.2
        WTPXRP15 Z9.2
        WTPXRP16 Z9.2
        WTPXRP17 Z9.2
        WTPXRP18 Z9.2
        WTPXRP19 Z9.2
        WTPXRP20 Z9.2
        WTPXRP21 Z9.2
        WTPXRP22 Z9.2
        WTPXRP23 Z9.2
        WTPXRP24 Z9.2
        WTPXRP25 Z9.2
        WTPXRP26 Z9.2
        WTPXRP27 Z9.2
        WTPXRP28 Z9.2
        WTPXRP29 Z9.2
        WTPXRP30 Z9.2
        WTPXRP31 Z9.2
        WTPXRP32 Z9.2
        WTPXRP33 Z9.2
        WTPXRP34 Z9.2
        WTPXRP35 Z9.2
        WTPXRP36 Z9.2
        WTPXRP37 Z9.2
        WTPXRP38 Z9.2
        WTPXRP39 Z9.2
        WTPXRP40 Z9.2
        WTPXRP41 Z9.2
        WTPXRP42 Z9.2
        WTPXRP43 Z9.2
        WTPXRP44 Z9.2
        WTPXRP45 Z9.2
        WTPXRP46 Z9.2
        WTPXRP47 Z9.2
        WTPXRP48 Z9.2
        WTPXRP49 Z9.2
        WTPXRP50 Z9.2
        WTPXRP51 Z9.2
        WTPXRP52 Z9.2
        PHPFAST  8.2
        WCP      8.2
        WCPSI    8.2
        LMPPCNT  8.2
        MOPPCNT  8.2
        GRPPCNT  8.2
        LMP      8.2
        MOP      7.2
        GRP      8.2
        RCP      7.2
        RCPSI    7.2
        HGP      8.2
        HGPSI    7.1
        HTP      8.2
        HTPSI    9.3
        MVPSI    8.2
        MCPSI    8.2
        MHP      8.2
        MHPSI    7.1
        RWP      8.2
        RWPSI    11.4
        PLP      7.1
        PLPSI    7.1
        DWP      8.2
        PVPSI    8.2
        PBP      6.1
        PBPSI    9.3
        EPPSI    8.2
        FEPSI    8.2
        TIPSI    9.2
        PXP      6.1
        FOP      7.1
        FOPSI    7.1
        RBPSI    8.1
        VBPSI    11.2
        VCP      7.2
        VCPSI    9.2
        ICPSI    7.2
        CAPSI    7.2
        SEPSI    7.2
        VAPSI    7.2
        VEPSI    9.2
        ACPSI    7.2
        BCPSI    8.2
        BXPSI    7.2
        LUPSI    7.2
        LYPSI    7.2
        REPSI    7.2
        COP      9.3
        TCP      8.
        TCPSI    8.2
        TGP      8.
        TGPSI    8.2
        LCP      8.
        LCPSI    7.2
        HDP      8.
        HDPSI    7.2
        AAP      8.
        AAPSI    7.2
        ABP      8.
        ABPSI    7.2
        LPP      8.
        LPPSI    7.2
        FHPSI    7.1
        LHPSI    6.1
        FBP      8.
        FBPSI    7.2
        CRP      8.2
        TEP      10.3
        AHP      2.
        HBP      2.
        SSP      2.
        SAP      2.
        HCP      2.
        DHP      2.
        H1P      8.
        H2P      8.
        RUP      8.2
        RUPUNIT  8.
        VRP      8.2
        RFP      8.
        L1P      5.2
        HPP      2.
        NAPSI    7.1
        SKPSI    7.2
        CLPSI    7.1
        C3PSI    8.
        SCP      6.1
        SCPSI    9.3
        PSP      6.1
        PSPSI    9.3
        UAP      6.1
        UAPSI    7.1
        SGP      8.
        SGPSI    8.2
        BUP      8.
        BUPSI    8.2
        TBP      6.1
        TBPSI    9.2
        CEP      6.1
        CEPSI    8.1
        SFP      8.
        SFPSI    6.1
        CHP      8.
        CHPSI    10.3
        TRP      8.
        TRPSI    10.3
        ASPSI    8.
        ATPSI    8.
        GGPSI    8.
        LDPSI    8.
        APPSI    8.
        TPP      6.1
        TPPSI    8.
        AMP      5.1
        AMPSI    8.
        GBP      5.1
        GBPSI    8.
        OSPSI    8.
        GHP      6.1
        GHPMETH  3.
        G1P      7.1
        G1PSI    10.3
        G1PCODE  2.
        G1PTIM1  4.
        G1PTIM2  4.
        G2P      7.1
        G2PSI    10.3
        C1P      9.3
        C1PSI    9.3
        C2P      10.3
        C2PSI    10.3
        I1P      9.2
        I1PSI    10.2
        I2P      9.2
        I2PSI    10.2
        UDP      8.2
        UDPSI    9.2
        URP      7.1
        URPSI    6.1
        UBP      8.1
        UIP      9.1
      ;

     INPUT
        SEQN     1-5
        DMPFSEQ  6-10
        DMPSTAT  11
        DMARETHN 12
        DMARACER 13
        DMAETHNR 14
        HSSEX    15
        HSAGEIR  16-17
        HSAGEU   18
        HSAITMOR 19-22
        HSFSIZER 23-24
        HSHSIZER 25-26
        DMPCNTYR 27-29
        DMPFIPSR 30-31
        DMPMETRO 32
        DMPCREGN 33
        DMPPIR   34-39
        SDPPHASE 40
        SDPPSU6  41
        SDPSTRA6 42-43
        SDPPSU1  44
        SDPSTRA1 45-46
        SDPPSU2  47
        SDPSTRA2 48-49
        WTPFQX6  50-58
        WTPFEX6  59-67
        WTPFHX6  68-76
        WTPFALG6 77-85
        WTPFCNS6 86-94
        WTPFSD6  95-103
        WTPFMD6  104-112
        WTPFHSD6 113-121
        WTPFHMD6 122-130
        WTPFQX1  131-139
        WTPFEX1  140-148
        WTPFHX1  149-157
        WTPFALG1 158-166
        WTPFCNS1 167-175
        WTPFSD1  176-184
        WTPFMD1  185-193
        WTPFHSD1 194-202
        WTPFHMD1 203-211
        WTPFQX2  212-220
        WTPFEX2  221-229
        WTPFHX2  230-238
        WTPFALG2 239-247
        WTPFCNS2 248-256
        WTPFSD2  257-265
        WTPFMD2  266-274
        WTPFHSD2 275-283
        WTPFHMD2 284-292
        WTPQRP1  293-301
        WTPQRP2  302-310
        WTPQRP3  311-319
        WTPQRP4  320-328
        WTPQRP5  329-337
        WTPQRP6  338-346
        WTPQRP7  347-355
        WTPQRP8  356-364
        WTPQRP9  365-373
        WTPQRP10 374-382
        WTPQRP11 383-391
        WTPQRP12 392-400
        WTPQRP13 401-409
        WTPQRP14 410-418
        WTPQRP15 419-427
        WTPQRP16 428-436
        WTPQRP17 437-445
        WTPQRP18 446-454
        WTPQRP19 455-463
        WTPQRP20 464-472
        WTPQRP21 473-481
        WTPQRP22 482-490
        WTPQRP23 491-499
        WTPQRP24 500-508
        WTPQRP25 509-517
        WTPQRP26 518-526
        WTPQRP27 527-535
        WTPQRP28 536-544
        WTPQRP29 545-553
        WTPQRP30 554-562
        WTPQRP31 563-571
        WTPQRP32 572-580
        WTPQRP33 581-589
        WTPQRP34 590-598
        WTPQRP35 599-607
        WTPQRP36 608-616
        WTPQRP37 617-625
        WTPQRP38 626-634
        WTPQRP39 635-643
        WTPQRP40 644-652
        WTPQRP41 653-661
        WTPQRP42 662-670
        WTPQRP43 671-679
        WTPQRP44 680-688
        WTPQRP45 689-697
        WTPQRP46 698-706
        WTPQRP47 707-715
        WTPQRP48 716-724
        WTPQRP49 725-733
        WTPQRP50 734-742
        WTPQRP51 743-751
        WTPQRP52 752-760
        WTPXRP1  761-769
        WTPXRP2  770-778
        WTPXRP3  779-787
        WTPXRP4  788-796
        WTPXRP5  797-805
        WTPXRP6  806-814
        WTPXRP7  815-823
        WTPXRP8  824-832
        WTPXRP9  833-841
        WTPXRP10 842-850
        WTPXRP11 851-859
        WTPXRP12 860-868
        WTPXRP13 869-877
        WTPXRP14 878-886
        WTPXRP15 887-895
        WTPXRP16 896-904
        WTPXRP17 905-913
        WTPXRP18 914-922
        WTPXRP19 923-931
        WTPXRP20 932-940
        WTPXRP21 941-949
        WTPXRP22 950-958
        WTPXRP23 959-967
        WTPXRP24 968-976
        WTPXRP25 977-985
        WTPXRP26 986-994
        WTPXRP27 995-1003
        WTPXRP28 1004-1012
        WTPXRP29 1013-1021
        WTPXRP30 1022-1030
        WTPXRP31 1031-1039
        WTPXRP32 1040-1048
        WTPXRP33 1049-1057
        WTPXRP34 1058-1066
        WTPXRP35 1067-1075
        WTPXRP36 1076-1084
        WTPXRP37 1085-1093
        WTPXRP38 1094-1102
        WTPXRP39 1103-1111
        WTPXRP40 1112-1120
        WTPXRP41 1121-1129
        WTPXRP42 1130-1138
        WTPXRP43 1139-1147
        WTPXRP44 1148-1156
        WTPXRP45 1157-1165
        WTPXRP46 1166-1174
        WTPXRP47 1175-1183
        WTPXRP48 1184-1192
        WTPXRP49 1193-1201
        WTPXRP50 1202-1210
        WTPXRP51 1211-1219
        WTPXRP52 1220-1228
        HYAITMO  1229-1232
        MXPLANG  1233
        MXPSESSR 1234
        MXPTIDW  1235
        MXPAXTMR 1236-1239
        HXPTIDW  1240
        HXPAXTMR 1241-1244
        HXPSESSR 1245
        PHPLANG  1246
        PHPHEMO  1247
        PHPCHM2  1248
        PHPINSU  1249
        PHPSNTI  1250-1254
        PHPSNDA  1255
        PHPDRIN  1256
        PHPDRTI  1257-1261
        PHPDRDA  1262
        PHPFAST  1263-1267
        PHPBEST  1268-1272
        WCP      1273-1277
        WCPSI    1278-1282
        LMPPCNT  1283-1287
        MOPPCNT  1288-1292
        GRPPCNT  1293-1297
        LMP      1298-1302
        MOP      1303-1306
        GRP      1307-1311
        RCP      1312-1315
        RCPSI    1316-1319
        HGP      1320-1324
        HGPSI    1325-1329
        HTP      1330-1334
        HTPSI    1335-1339
        MVPSI    1340-1344
        MCPSI    1345-1349
        MHP      1350-1354
        MHPSI    1355-1359
        RWP      1360-1364
        RWPSI    1365-1370
        PLP      1371-1375
        PLPSI    1376-1380
        DWP      1381-1385
        PVPSI    1386-1390
        GRPDIF   1391-1393
        LMPDIF   1394-1396
        MOPDIF   1397-1398
        EOP      1399-1400
        BOP      1401-1402
        BLP      1403
        PRP      1404
        MEP      1405
        MLP      1406
        BAP      1407-1408
        LAP      1409-1410
        ANP      1411
        BSP      1412
        HZP      1413
        PKP      1414
        POP      1415
        MRP      1416
        MIP      1417
        SIP      1418
        SHP      1419
        TTP      1420
        TXP      1421
        VUP      1422
        PBP      1423-1426
        PBPSI    1427-1431
        EPP      1432-1435
        EPPSI    1436-1440
        FEP      1441-1443
        FEPSI    1444-1448
        TIP      1449-1452
        TIPSI    1453-1458
        PXP      1459-1462
        FRP      1463-1466
        FRPSI    1467-1470
        FOP      1471-1475
        FOPSI    1476-1480
        RBP      1481-1484
        RBPSI    1485-1490
        VBP      1491-1496
        VBPSI    1497-1504
        VCP      1505-1508
        VCPSI    1509-1514
        ICPSI    1515-1518
        CAPSI    1519-1522
        SEP      1523-1526
        SEPSI    1527-1530
        VAP      1531-1533
        VAPSI    1534-1537
        VEP      1538-1542
        VEPSI    1543-1548
        ACP      1549-1551
        ACPSI    1552-1555
        BCP      1556-1559
        BCPSI    1560-1564
        BXP      1565-1567
        BXPSI    1568-1571
        LUP      1572-1574
        LUPSI    1575-1578
        LYP      1579-1581
        LYPSI    1582-1585
        REP      1586-1588
        REPSI    1589-1592
        COP      1593-1597
        TCP      1598-1600
        TCPSI    1601-1605
        TGP      1606-1609
        TGPSI    1610-1614
        LCP      1615-1617
        LCPSI    1618-1621
        HDP      1622-1624
        HDPSI    1625-1628
        AAP      1629-1631
        AAPSI    1632-1635
        ABP      1636-1638
        ABPSI    1639-1642
        LPP      1643-1645
        LPPSI    1646-1649
        FHPSI    1650-1654
        LHPSI    1655-1658
        FBP      1659-1662
        FBPSI    1663-1666
        CRP      1667-1671
        TEP      1672-1677
        AHP      1678
        HBP      1679
        SSP      1680-1681
        SAP      1682
        HCP      1683
        DHP      1684
        H1P      1685
        H2P      1686
        RUP      1687-1691
        RUPUNIT  1692-1695
        VRP      1696-1700
        TOP      1701-1703
        RFP      1704-1708
        L1P      1709-1713
        HPP      1714
        NAPSI    1715-1719
        SKPSI    1720-1723
        CLPSI    1724-1728
        C3PSI    1729-1730
        SCP      1731-1734
        SCPSI    1735-1739
        PSP      1740-1743
        PSPSI    1744-1748
        UAP      1749-1752
        UAPSI    1753-1757
        SGP      1758-1760
        SGPSI    1761-1765
        BUP      1766-1768
        BUPSI    1769-1773
        TBP      1774-1777
        TBPSI    1778-1783
        CEP      1784-1787
        CEPSI    1788-1793
        SFP      1794-1796
        SFPSI    1797-1800
        CHP      1801-1804
        CHPSI    1805-1810
        TRP      1811-1814
        TRPSI    1815-1820
        ASPSI    1821-1823
        ATPSI    1824-1826
        GGPSI    1827-1830
        LDPSI    1831-1834
        APPSI    1835-1838
        TPP      1839-1842
        TPPSI    1843-1845
        AMP      1846-1848
        AMPSI    1849-1851
        GBP      1852-1854
        GBPSI    1855-1857
        OSPSI    1858-1860
        GHP      1861-1864
        GHPMETH  1865
        G1P      1866-1870
        G1PSI    1871-1876
        G1PCODE  1877-1878
        G1PTIM1  1879-1881
        G1PTIM2  1882-1884
        G2P      1885-1889
        G2PSI    1890-1895
        C1P      1896-1900
        C1PSI    1901-1905
        C2P      1906-1911
        C2PSI    1912-1917
        I1P      1918-1923
        I1PSI    1924-1930
        I1P2PFLG 1931
        I2P      1932-1937
        I2PSI    1938-1944
        UDP      1945-1949
        UDPSI    1950-1955
        URP      1956-1960
        URPSI    1961-1964
        UBP      1965-1970
        UIP      1971-1977
     ;

      LABEL
        SEQN     = "Sample person identification number"
        DMPFSEQ  = "Family sequence number"
        DMPSTAT  = "Examination/interview Status"
        DMARETHN = "Race-ethnicity"
        DMARACER = "Race"
        DMAETHNR = "Ethnicity"
        HSSEX    = "Sex"
        HSAGEIR  = "Age at interview (Screener)"
        HSAGEU   = "Age at interview - unit (Screener)"
        HSAITMOR = "Age in months at interview (screener)"
        HSFSIZER = "Family size (persons in family)"
        HSHSIZER = "Household size (persons in dwelling)"
        DMPCNTYR = "County code"
        DMPFIPSR = "FIPS code for State"
        DMPMETRO = "Rural/urban code based on USDA code"
        DMPCREGN = "Census region, weighting(Texas in south)"
        DMPPIR   = "Poverty Income Ratio (unimputed income)"
        SDPPHASE = "Phase of NHANES III survey"
        SDPPSU6  = "Total NHANES III pseudo-PSU"
        SDPSTRA6 = "Total NHANES III pseudo-stratum"
        SDPPSU1  = "Pseudo-PSU for phase 1"
        SDPSTRA1 = "Pseudo-stratum for phase 1"
        SDPPSU2  = "Pseudo-PSU for phase 2"
        SDPSTRA2 = "Pseudo-stratum for phase 2"
        WTPFQX6  = "Total interviewed sample final weight"
        WTPFEX6  = "Total MEC-examined sample final weight"
        WTPFHX6  = "Total M+H examined sample final weight"
        WTPFALG6 = "Total allergy subsample final weight"
        WTPFCNS6 = "Total CNS subsample final weight"
        WTPFSD6  = "Total morning subsample final wgt"
        WTPFMD6  = "Total afternoon/eve subsample final wgt"
        WTPFHSD6 = "Total M+H morning subsample final wgt"
        WTPFHMD6 = "Total M+H afternoon subsample final wgt"
        WTPFQX1  = "Phase 1 interviewed sample final wgt"
        WTPFEX1  = "Phase 1 MEC examined sample final wgt"
        WTPFHX1  = "Phase 1 M+H examined sample final wgt"
        WTPFALG1 = "Phase 1 allergy subsample final wgt"
        WTPFCNS1 = "Phase 1 CNS subsample final wgt"
        WTPFSD1  = "Phase 1 morning sess subsample final wgt"
        WTPFMD1  = "Phase 1 aft/eve subsample final wgt"
        WTPFHSD1 = "Phase 1 morning M+H subsample final wgt"
        WTPFHMD1 = "Phase 1 aft/eve M+H subsample final wgt"
        WTPFQX2  = "Phase 2 interviewed sample final wgt"
        WTPFEX2  = "Phase 2 MEC examined sample final wgt"
        WTPFHX2  = "Phase 2 M+H examined sample final wgt"
        WTPFALG2 = "Phase 2 allergy subsample final wgt"
        WTPFCNS2 = "Phase 2 CNS subsample final wgt"
        WTPFSD2  = "Phase 2 morning sess subsample final wgt"
        WTPFMD2  = "Phase 2 aft/eve subsample final wgt"
        WTPFHSD2 = "Phase 2 morning M+H subsample final wgt"
        WTPFHMD2 = "Phase 2 aft/eve M+H subsample final wgt"
        WTPQRP1  = "Replicate 1 final interview weight"
        WTPQRP2  = "Replicate 2 final interview weight"
        WTPQRP3  = "Replicate 3 final interview weight"
        WTPQRP4  = "Replicate 4 final interview weight"
        WTPQRP5  = "Replicate 5 final interview weight"
        WTPQRP6  = "Replicate 6 final interview weight"
        WTPQRP7  = "Replicate 7 final interview weight"
        WTPQRP8  = "Replicate 8 final interview weight"
        WTPQRP9  = "Replicate 9 final interview weight"
        WTPQRP10 = "Replicate 10 final interview weight"
        WTPQRP11 = "Replicate 11 final interview weight"
        WTPQRP12 = "Replicate 12 final interview weight"
        WTPQRP13 = "Replicate 13 final interview weight"
        WTPQRP14 = "Replicate 14 final interview weight"
        WTPQRP15 = "Replicate 15 final interview weight"
        WTPQRP16 = "Replicate 16 final interview weight"
        WTPQRP17 = "Replicate 17 final interview weight"
        WTPQRP18 = "Replicate 18 final interview weight"
        WTPQRP19 = "Replicate 19 final interview weight"
        WTPQRP20 = "Replicate 20 final interview weight"
        WTPQRP21 = "Replicate 21 final interview weight"
        WTPQRP22 = "Replicate 22 final interview weight"
        WTPQRP23 = "Replicate 23 final interview weight"
        WTPQRP24 = "Replicate 24 final interview weight"
        WTPQRP25 = "Replicate 25 final interview weight"
        WTPQRP26 = "Replicate 26 final interview weight"
        WTPQRP27 = "Replicate 27 final interview weight"
        WTPQRP28 = "Replicate 28 final interview weight"
        WTPQRP29 = "Replicate 29 final interview weight"
        WTPQRP30 = "Replicate 30 final interview weight"
        WTPQRP31 = "Replicate 31 final interview weight"
        WTPQRP32 = "Replicate 32 final interview weight"
        WTPQRP33 = "Replicate 33 final interview weight"
        WTPQRP34 = "Replicate 34 final interview weight"
        WTPQRP35 = "Replicate 35 final interview weight"
        WTPQRP36 = "Replicate 36 final interview weight"
        WTPQRP37 = "Replicate 37 final interview weight"
        WTPQRP38 = "Replicate 38 final interview weight"
        WTPQRP39 = "Replicate 39 final interview weight"
        WTPQRP40 = "Replicate 40 final interview weight"
        WTPQRP41 = "Replicate 41 final interview weight"
        WTPQRP42 = "Replicate 42 final interview weight"
        WTPQRP43 = "Replicate 43 final interview weight"
        WTPQRP44 = "Replicate 44 final interview weight"
        WTPQRP45 = "Replicate 45 final interview weight"
        WTPQRP46 = "Replicate 46 final interview weight"
        WTPQRP47 = "Replicate 47 final interview weight"
        WTPQRP48 = "Replicate 48 final interview weight"
        WTPQRP49 = "Replicate 49 final interview weight"
        WTPQRP50 = "Replicate 50 final interview weight"
        WTPQRP51 = "Replicate 51 final interview weight"
        WTPQRP52 = "Replicate 52 final interview weight"
        WTPXRP1  = "Replicate 1 final exam weight"
        WTPXRP2  = "Replicate 2 final exam weight"
        WTPXRP3  = "Replicate 3 final exam weight"
        WTPXRP4  = "Replicate 4 final exam weight"
        WTPXRP5  = "Replicate 5 final exam weight"
        WTPXRP6  = "Replicate 6 final exam weight"
        WTPXRP7  = "Replicate 7 final exam weight"
        WTPXRP8  = "Replicate 8 final exam weight"
        WTPXRP9  = "Replicate 9 final exam weight"
        WTPXRP10 = "Replicate 10 final exam weight"
        WTPXRP11 = "Replicate 11 final exam weight"
        WTPXRP12 = "Replicate 12 final exam weight"
        WTPXRP13 = "Replicate 13 final exam weight"
        WTPXRP14 = "Replicate 14 final exam weight"
        WTPXRP15 = "Replicate 15 final exam weight"
        WTPXRP16 = "Replicate 16 final exam weight"
        WTPXRP17 = "Replicate 17 final exam weight"
        WTPXRP18 = "Replicate 18 final exam weight"
        WTPXRP19 = "Replicate 19 final exam weight"
        WTPXRP20 = "Replicate 20 final exam weight"
        WTPXRP21 = "Replicate 21 final exam weight"
        WTPXRP22 = "Replicate 22 final exam weight"
        WTPXRP23 = "Replicate 23 final exam weight"
        WTPXRP24 = "Replicate 24 final exam weight"
        WTPXRP25 = "Replicate 25 final exam weight"
        WTPXRP26 = "Replicate 26 final exam weight"
        WTPXRP27 = "Replicate 27 final exam weight"
        WTPXRP28 = "Replicate 28 final exam weight"
        WTPXRP29 = "Replicate 29 final exam weight"
        WTPXRP30 = "Replicate 30 final exam weight"
        WTPXRP31 = "Replicate 31 final exam weight"
        WTPXRP32 = "Replicate 32 final exam weight"
        WTPXRP33 = "Replicate 33 final exam weight"
        WTPXRP34 = "Replicate 34 final exam weight"
        WTPXRP35 = "Replicate 35 final exam weight"
        WTPXRP36 = "Replicate 36 final exam weight"
        WTPXRP37 = "Replicate 37 final exam weight"
        WTPXRP38 = "Replicate 38 final exam weight"
        WTPXRP39 = "Replicate 39 final exam weight"
        WTPXRP40 = "Replicate 40 final exam weight"
        WTPXRP41 = "Replicate 41 final exam weight"
        WTPXRP42 = "Replicate 42 final exam weight"
        WTPXRP43 = "Replicate 43 final exam weight"
        WTPXRP44 = "Replicate 44 final exam weight"
        WTPXRP45 = "Replicate 45 final exam weight"
        WTPXRP46 = "Replicate 46 final exam weight"
        WTPXRP47 = "Replicate 47 final exam weight"
        WTPXRP48 = "Replicate 48 final exam weight"
        WTPXRP49 = "Replicate 49 final exam weight"
        WTPXRP50 = "Replicate 50 final exam weight"
        WTPXRP51 = "Replicate 51 final exam weight"
        WTPXRP52 = "Replicate 52 final exam weight"
        HYAITMO  = "Age in months at youth interview"
        MXPLANG  = "Language used by SP in MEC"
        MXPSESSR = "Session for MEC examination"
        MXPTIDW  = "Day of week of MEC exam"
        MXPAXTMR = "Age in months at MEC exam"
        HXPTIDW  = "Day of week of home exam"
        HXPAXTMR = "Age in months at home exam"
        HXPSESSR = "Session for home examination"
        PHPLANG  = "Language"
        PHPHEMO  = "Do you have hemophilia?"
        PHPCHM2  = "Recent chemo/within the past four weeks"
        PHPINSU  = "Are you currently taking insulin?"
        PHPSNTI  = "Time participant last ate"
        PHPSNDA  = "Day participant last ate"
        PHPDRIN  = "Have you had anything to drink?"
        PHPDRTI  = "Time participant last drank"
        PHPDRDA  = "Day participant last drank"
        PHPFAST  = "Length of calculated fast (in hours)"
        PHPBEST  = "Time of venipuncture"
        WCP      = "White blood cell count"
        WCPSI    = "White blood cell count: SI"
        LMPPCNT  = "Lymphocyte percent (Coulter)"
        MOPPCNT  = "Mononuclear percent (Coulter)"
        GRPPCNT  = "Granulocyte percent (Coulter)"
        LMP      = "Lymphocyte number (Coulter)"
        MOP      = "Mononuclear number (Coulter)"
        GRP      = "Granulocyte number (Coulter)"
        RCP      = "Red blood cell count"
        RCPSI    = "Red blood cell count:  SI"
        HGP      = "Hemoglobin (g/dL)"
        HGPSI    = "Hemoglobin:  SI (g/L)"
        HTP      = "Hematocrit (%)"
        HTPSI    = "Hematocrit:  SI (L/L=1)"
        MVPSI    = "Mean cell volume:  SI (fL)"
        MCPSI    = "Mean cell hemoglobin:  SI (pg)"
        MHP      = "Mean cell hemoglobin concentration"
        MHPSI    = "Mean cell hemoglobin concentration: SI"
        RWP      = "Red cell distribution width (%)"
        RWPSI    = "Red cell distribution width:SI(fraction)"
        PLP      = "Platelet count"
        PLPSI    = "Platelet count:  SI"
        DWP      = "Platelet distribution width (%)"
        PVPSI    = "Mean platelet volume:  SI (fL)"
        GRPDIF   = "Segment neutrophil(percent of 100 cells)"
        LMPDIF   = "Lymphocytes (percent of 100 cells)"
        MOPDIF   = "Monocytes (percent of 100 cells)"
        EOP      = "Eosinophils (percent of 100 cells)"
        BOP      = "Basophils (percent of 100 cells)"
        BLP      = "Blasts (percent of 100 cells)"
        PRP      = "Promyelocytes (percent of 100 cells)"
        MEP      = "Metamyelocytes (percent of 100 cells)"
        MLP      = "Myelocytes (percent of 100 cells)"
        BAP      = "Bands (percent of 100 cells)"
        LAP      = "Atyp lymphocytes (percent of 100 cells)"
        ANP      = "Anisocytosis (variation of cell size)"
        BSP      = "Basophilic stippling"
        HZP      = "Hypochromia (stain intensity of cell)"
        PKP      = "Poikilocytosis (cell shape variation)"
        POP      = "Polychromatophilia(bluish color of cell)"
        MRP      = "Macrocytosis (large cell prevalence)"
        MIP      = "Microcytosis (small cell prevalence)"
        SIP      = "Sickle cells"
        SHP      = "Spherocytosis"
        TTP      = "Target cells"
        TXP      = "Toxic granulation"
        VUP      = "Vacuolated cells"
        PBP      = "Lead (ug/dL)"
        PBPSI    = "Lead:  SI (umol/L)"
        EPP      = "Erythrocyte protoporphyrin (ug/dL)"
        EPPSI    = "Erythrocyte protoporphyrin:  SI (umol/L)"
        FEP      = "Serum iron (ug/dL)"
        FEPSI    = "Serum iron:  SI (umol/L)"
        TIP      = "Serum TIBC (ug/dL)"
        TIPSI    = "Serum TIBC:  SI (umol/L)"
        PXP      = "Serum transferrin saturation (%)"
        FRP      = "Serum ferritin (ng/mL)"
        FRPSI    = "Serum ferritin:  SI (ug/L)"
        FOP      = "Serum folate (ng/mL)"
        FOPSI    = "Serum folate:  SI (nmol/L)"
        RBP      = "RBC folate (ng/mL)"
        RBPSI    = "RBC folate:  SI (nmol/L)"
        VBP      = "Serum vitamin B12 (pg/mL)"
        VBPSI    = "Serum vitamin B12:  SI (pmol/L)"
        VCP      = "Serum vitamin C (mg/dL)"
        VCPSI    = "Serum vitamin C:  SI (mmol/L)"
        ICPSI    = "Serum normalized calcium:  SI (mmol/L)"
        CAPSI    = "Serum total calcium:  SI (mmol/L)"
        SEP      = "Serum selenium (ng/mL)"
        SEPSI    = "Serum selenium:  SI (nmol/L)"
        VAP      = "Serum vitamin A (ug/dL)"
        VAPSI    = "Serum vitamin A:  SI (umol/L)"
        VEP      = "Serum vitamin E (ug/dL)"
        VEPSI    = "Serum vitamin E:  SI (umol/L)"
        ACP      = "Serum alpha carotene (ug/dL)"
        ACPSI    = "Serum alpha carotene:  SI (umol/L)"
        BCP      = "Serum beta carotene (ug/dL)"
        BCPSI    = "Serum beta carotene: SI (umol/L)"
        BXP      = "Serum beta cryptoxanthin (ug/dL)"
        BXPSI    = "Serum beta cryptoxanthin:  SI (umol/L)"
        LUP      = "Serum lutein/zeaxanthin (ug/dL)"
        LUPSI    = "Serum lutein/zeaxanthin:  SI (umol/L)"
        LYP      = "Serum lycopene (ug/dL)"
        LYPSI    = "Serum lycopene:  SI (umol/L)"
        REP      = "Serum sum retinyl esters (ug/dL)"
        REPSI    = "Serum sum retinyl esters:  SI (umol/L)"
        COP      = "Serum cotinine (ng/mL)"
        TCP      = "Serum cholesterol (mg/dL)"
        TCPSI    = "Serum cholesterol:  SI (mmol/L)"
        TGP      = "Serum triglycerides (mg/dL)"
        TGPSI    = "Serum triglycerides:  SI (mmol/L)"
        LCP      = "Serum LDL cholesterol (mg/dL)"
        LCPSI    = "Serum LDL cholesterol:  SI (mmol/L)"
        HDP      = "Serum HDL cholesterol (mg/dL)"
        HDPSI    = "Serum HDL cholesterol:  SI (mmol/L)"
        AAP      = "Serum apolipoprotein AI (mg/dL)"
        AAPSI    = "Serum apolipoprotein AI: SI (g/L)"
        ABP      = "Serum apolipoprotein B (mg/dL)"
        ABPSI    = "Serum apolipoprotein B: SI (g/L)"
        LPP      = "Serum lipoprotein(a) (mg/dL)"
        LPPSI    = "Serum lipoprotein(a):  SI (g/L)"
        FHPSI    = "Serum FSH:  SI (IU/L)"
        LHPSI    = "Serum luteinizing hormone: SI (IU/L)"
        FBP      = "Plasma fibrinogen (mg/dL)"
        FBPSI    = "Plasma fibrinogen:  SI (g/L)"
        CRP      = "Serum C-reactive protein (mg/dL)"
        TEP      = "Serum tetanus antibody (U/mL)"
        AHP      = "Serum hepatitis A antibody"
        HBP      = "Serum hepatitis B core antibody"
        SSP      = "Serum hepatitis B surface antibody"
        SAP      = "Serum hepatitis B surface antigen"
        HCP      = "Serum hepatitis C antibody"
        DHP      = "Serum hepatitis D antibody"
        H1P      = "Serum herpes I antibody"
        H2P      = "Serum herpes II antibody"
        RUP      = "Serum rubella antibody"
        RUPUNIT  = "Serum rubells antibody (IU)"
        VRP      = "Serum varicella antibody"
        TOP      = "Serum toxoplasmosis antibody"
        RFP      = "Serum rheumatoid factor antibody"
        L1P      = "Serum latex antibody (IU/mL)"
        HPP      = "Serum helicobacter pylori antibody"
        NAPSI    = "Serum sodium:  SI (mmol/L)"
        SKPSI    = "Serum potassium:  SI (mmol/L)"
        CLPSI    = "Serum chloride:  SI (mmol/L)"
        C3PSI    = "Serum bicarbonate:  SI (mmol/L)"
        SCP      = "Serum total calcium (mg/dL)"
        SCPSI    = "Serum total calcium: SI (mmol/L)"
        PSP      = "Serum phosphorus (mg/dL)"
        PSPSI    = "Serum phosphorus: SI (mmol/L)"
        UAP      = "Serum uric acid (mg/dL)"
        UAPSI    = "Serum uric acid:  SI (umol/L)"
        SGP      = "Serum glucose (mg/dL)"
        SGPSI    = "Serum glucose:  SI (mmol/L)"
        BUP      = "Serum blood urea nitrogen (mg/dL)"
        BUPSI    = "Serum blood urea nitrogen:  SI (mmol/L)"
        TBP      = "Serum total bilirubin (mg/dL)"
        TBPSI    = "Serum total bilirubin:  SI (umol/L)"
        CEP      = "Serum creatinine (mg/dL)"
        CEPSI    = "Serum creatinine:  SI (umol/L)"
        SFP      = "Serum iron (ug/dL)"
        SFPSI    = "Serum iron:  SI (umol/L)"
        CHP      = "Serum cholesterol (mg/dL)"
        CHPSI    = "Serum cholesterol:  SI (mmol/L)"
        TRP      = "Serum triglycerides (mg/dL)"
        TRPSI    = "Serum triglycerides:  SI (mmol/L)"
        ASPSI    = "Aspartate aminotransferase: SI(U/L)"
        ATPSI    = "Alanine aminotransferase:  SI (U/L)"
        GGPSI    = "Gamma glutamyl transferase: SI(U/L)"
        LDPSI    = "Serum lactate dehydrogenase:  SI (U/L)"
        APPSI    = "Serum alkaline phosphatase:  SI (U/L)"
        TPP      = "Serum total protein (g/dL)"
        TPPSI    = "Serum total protein:  SI (g/L)"
        AMP      = "Serum albumin (g/dL)"
        AMPSI    = "Serum albumin:  SI (g/L)"
        GBP      = "Serum globulin (g/dL)"
        GBPSI    = "Serum globulin:  SI (g/L)"
        OSPSI    = "Serum osmolality:  SI (mmol/Kg)"
        GHP      = "Glycated hemoglobin: (%)"
        GHPMETH  = "Glycated hemoglobin: test method"
        G1P      = "Plasma glucose (mg/dL)"
        G1PSI    = "Plasma glucose:  SI (mmol/L)"
        G1PCODE  = "Incomplete glucose test (OGTT) code"
        G1PTIM1  = "Minutes between drink and second draw"
        G1PTIM2  = "Minutes between first and second draw"
        G2P      = "Second plasma glucose (mg/dL)"
        G2PSI    = "Second plasma glucose: SI (mmol/L)"
        C1P      = "Serum C-peptide (pmol/mL)"
        C1PSI    = "Serum C-peptide: SI (nmol/L)"
        C2P      = "Second serum C-peptide (pmol/mL)"
        C2PSI    = "Second serum C-peptide: SI (nmol/L)"
        I1P      = "Serum insulin (uU/mL)"
        I1PSI    = "Serum insulin:  SI (pmol/L)"
        I1P2PFLG = "Serum insulin: test kit"
        I2P      = "Second serum insulin (uU/mL)"
        I2PSI    = "Second serum insulin: SI (pmol/L)"
        UDP      = "Urinary cadmium (ng/mL)"
        UDPSI    = "Urinary cadmium: SI (nmol/L)"
        URP      = "Urinary creatinine (mg/dL)"
        URPSI    = "Urinary creatinine: SI (mmol/L)"
        UBP      = "Urinary albumin (ug/mL)"
        UIP      = "Urinary iodine (ug/dL)"
      ;

