    FILENAME ADULT "D:\Questionnaire\DAT\ADULT.DAT" LRECL=3348;
    *** LRECL includes 2 positions for CRLF, assuming use of PC SAS;

    DATA WORK;
      INFILE ADULT MISSOVER;

      LENGTH
        SEQN      7
        DMPFSEQ   5
        DMPSTAT   3
        DMARETHN  3
        DMARACER  3
        DMAETHNR  3
        HSSEX     3
        HSDOIMO   3
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
        MXPTIMO   3
        MXPTIDW   3
        MXPAXTMR  4
        HXPSESSR  3
        HXPTIMO   3
        HXPTIDW   3
        HXPAXTMR  4
        HFVERS    3
        HFINTVR   4
        HFLANG    3
        HFA6XCR   3
        HFA8R     3
        HFA12     3
        HFA13     3
        HFB1      3
        HFB2      3
        HFB3      3
        HFB4      3
        HFB5      3
        HFB6      3
        HFB7      3
        HFB8      3
        HFB9      3
        HFB10     3
        HFB11     3
        HFB11A    3
        HFB11B    3
        HFB11C    3
        HFB11D    3
        HFB11E    3
        HFB12     3
        HFB13     3
        HFB14AB   3
        HFB14AC   3
        HFB14AD   3
        HFB14AE   3
        HFB14AF   3
        HFB14AG   3
        HFB14AI   3
        HFB14BA   3
        HFB14BB   3
        HFB14BC   3
        HFB14BD   3
        HFB14BE   3
        HFB14BF   3
        HFB14BG   3
        HFB14BH   3
        HFB16A    3
        HFB16B    3
        HFB17     3
        HFC1      3
        HFC2S     3
        HFC3S     3
        HFC4S     3
        HFC5S     3
        HFC6A     3
        HFC6A1    3
        HFC6B     3
        HFC6B1    3
        HFC6C     3
        HFC6C1    3
        HFC6D     3
        HFC6D1    3
        HFC6E     3
        HFC6E1    3
        HFC7S     4
        HFC8S     4
        HFC9      3
        HFC10     3
        HFC11     3
        HFD1      3
        HFD2      3
        HFD3      3
        HFD4      3
        HFD5      3
        HFD9R     3
        HFD10R    3
        HFD12     3
        HFE1      3
        HFE2      3
        HFE3      3
        HFE4      3
        HFE5      3
        HFE6      3
        HFE7      3
        HFE8A     3
        HFE8B     3
        HFE8C     3
        HFE8D     3
        HFE8E     3
        HFE9      3
        HFE10     3
        HFE11     3
        HFE12     3
        HFE13     3
        HFE14     3
        HFE15     3
        HFE16     3
        HFE17     3
        HFE18     3
        HFF1      3
        HFF2R     3
        HFF3A     3
        HFF3B     3
        HFF3C     3
        HFF3D     3
        HFF3E     3
        HFF3F     3
        HFF4      3
        HFF5      3
        HFF6A     3
        HFF6B     3
        HFF6C     3
        HFF6D     3
        HFF7      3
        HFF8      3
        HFF9      3
        HFF10     3
        HFF11     3
        HFF12A    3
        HFF12B    3
        HFF13A    3
        HFF13B    3
        HFF13DA   3
        HFF13DB   3
        HFF13E    3
        HFF14A    3
        HFF14B    3
        HFF14D    3
        HFF15A    3
        HFF15B    3
        HFF16A    3
        HFF16B    3
        HFF16D    3
        HFF16E    3
        HFF17A    3
        HFF17B    3
        HFF18     3
        HFF19R    3
        HFF20R    3
        HFHSEQN   7
        HFHAGER   3
        HFHSEX    3
        HFHFIPCR  3
        HFHEDUCR  3
        HFHEMPL   3
        HFHMRST   3
        HFHMLSV   3
        HFRELR    3
        HFAGERR   3
        HAVERS    3
        HAINTVR   4
        HALANG    3
        HAA1      3
        HAA2      3
        HAA2A     3
        HAA3      3
        HAA4      3
        HAA5      3
        HAA6A     3
        HAA6B     3
        HAA6C     3
        HAA6D     3
        HAB1      3
        HAB2      3
        HAB3      3
        HAB4      3
        HAB5      3
        HAB6S     3
        HAB7      3
        HAC1A     3
        HAC1B     3
        HAC1C     3
        HAC1D     3
        HAC1E     3
        HAC1F     3
        HAC1G     3
        HAC1H     3
        HAC1I     3
        HAC1J     3
        HAC1K     3
        HAC1L     3
        HAC1M     3
        HAC1N     3
        HAC1O     3
        HAC2E     3
        HAC2F     3
        HAC2H     3
        HAC2J     3
        HAC2K     3
        HAC3AR    3
        HAC3CR    3
        HAC3DR    3
        HAC3ER    3
        HAC3FR    3
        HAC3GR    3
        HAC3HR    3
        HAC3IR    3
        HAC3JR    3
        HAC3KR    3
        HAC3LR    3
        HAC3MR    3
        HAC3NR    3
        HAC3OR    3
        HAC3OS    3
        HAC4A     3
        HAC4B     3
        HAC5A1    3
        HAC5A2    3
        HAC5A3    3
        HAC5A4    3
        HAC5A5    3
        HAC5A6    3
        HAC5A7    3
        HAC5A8    3
        HAC5A9    3
        HAC5A10   3
        HAC5A11   3
        HAC5A12   3
        HAC5B1    3
        HAC5B2    3
        HAC5B3    3
        HAC5B4    3
        HAC5B5    3
        HAC5B6    3
        HAC5B7    3
        HAC5B8    3
        HAC5B9    3
        HAC5B10   3
        HAC5B11   3
        HAC5B12   3
        HAC6      3
        HAC7      3
        HAC7A     3
        HAC8      3
        HAD1      3
        HAD2      3
        HAD3      3
        HAD4      3
        HAD5R     3
        HAD6      3
        HAD7S     3
        HAD8      3
        HAD9S     3
        HAD10     3
        HAD11AS   3
        HAD11BS   3
        HAD12S    4
        HAD13S    4
        HAD14     3
        HAD15     3
        HAE1      3
        HAE2      3
        HAE3      3
        HAE4A     3
        HAE4B     3
        HAE4C     3
        HAE4D     3
        HAE4D1    3
        HAE4D2    3
        HAE4D3    3
        HAE4D4    3
        HAE4D5    3
        HAE4D6    3
        HAE5A     3
        HAE5B     3
        HAE5C     3
        HAE5D1    3
        HAE5D2    3
        HAE5D3    3
        HAE5D4    3
        HAE5D5    3
        HAE5D6    3
        HAE6      3
        HAE7      3
        HAE8A     3
        HAE8B     3
        HAE8C     3
        HAE8D     3
        HAE9A     3
        HAE9B     3
        HAE9C     3
        HAE9D     3
        HAE10     3
        HAE11A    3
        HAE11B    3
        HAE11C    3
        HAF1      3
        HAF2      3
        HAF3      3
        HAF4      3
        HAF5      3
        HAF6      3
        HAF7A     3
        HAF7B     3
        HAF7C     3
        HAF7D     3
        HAF7E     3
        HAF7F     3
        HAF7G     3
        HAF7H     3
        HAF9      3
        HAF10     3
        HAF11     3
        HAF12R    3
        HAF13R    3
        HAF14     3
        HAF15     3
        HAF16     3
        HAF17     3
        HAF18     3
        HAF19     3
        HAF20     3
        HAF21     3
        HAF22     3
        HAF23     3
        HAF24     3
        HAF25     3
        HAF26     3
        HAF27     3
        HAG1      3
        HAG2      3
        HAG3      3
        HAG4A     3
        HAG4B     3
        HAG4C     3
        HAG5A     3
        HAG5B     3
        HAG5C     3
        HAG6A     3
        HAG6B     3
        HAG6C     3
        HAG7      3
        HAG8A     3
        HAG8B     3
        HAG8C     3
        HAG8D     3
        HAG8E     3
        HAG8F     3
        HAG9AR    3
        HAG9BR    3
        HAG9CR    3
        HAG9DR    3
        HAG9ER    3
        HAG9FR    3
        HAG10A    3
        HAG10B    3
        HAG10C    3
        HAG10D    3
        HAG10E    3
        HAG10F    3
        HAG11     3
        HAG12     3
        HAG13     3
        HAG14     3
        HAG15     3
        HAG16     3
        HAG17A    3
        HAG17B    3
        HAG17C    3
        HAG17D    3
        HAG17E    3
        HAG17F    3
        HAG17G    3
        HAG17H    3
        HAG17I    3
        HAG17J    3
        HAG17K    3
        HAG17L    3
        HAG17M    3
        HAG17N    3
        HAG17O    3
        HAG17FF   3
        HAG17P    3
        HAG17Q    3
        HAG17R    3
        HAG17S    3
        HAG17T    3
        HAG17U    3
        HAG17V    3
        HAG17W    3
        HAG17X    3
        HAG17Y    3
        HAG17Z    3
        HAG17AA   3
        HAG17BB   3
        HAG17CC   3
        HAG17DD   3
        HAG17EE   3
        HAG18     3
        HAG19A    3
        HAG19B    3
        HAG19C    3
        HAG19D    3
        HAG19E    3
        HAG19F    3
        HAG19G    3
        HAG19H    3
        HAG19I    3
        HAG19J    3
        HAG19K    3
        HAG19L    3
        HAG19M    3
        HAG19N    3
        HAG19O    3
        HAG19FF   3
        HAG19P    3
        HAG19Q    3
        HAG19R    3
        HAG19S    3
        HAG19T    3
        HAG19U    3
        HAG19V    3
        HAG19W    3
        HAG19X    3
        HAG19Y    3
        HAG19Z    3
        HAG19AA   3
        HAG19BB   3
        HAG19CC   3
        HAG19DD   3
        HAG19EE   3
        HAG20     3
        HAG21     3
        HAG22     3
        HAG23     3
        HAG24     3
        HAG25     3
        HAG26     3
        HAG27     3
        HAG28     3
        HAG29     3
        HAG30     3
        HAG31     3
        HAG32     3
        HAG33     3
        HAG34     3
        HAH1      3
        HAH2      3
        HAH3      3
        HAH4      3
        HAH5      3
        HAH6      3
        HAH7      3
        HAH8      3
        HAH9      3
        HAH10     3
        HAH11     3
        HAH12     3
        HAH13     3
        HAH14     3
        HAH15     3
        HAH16     3
        HAH17     3
        HAJ0      3
        HAJ1      3
        HAJ2A     3
        HAJ2B     3
        HAJ2C     3
        HAJ3S     4
        HAJ4      3
        HAJ5      3
        HAJ6      3
        HAJ7      3
        HAJ8      3
        HAJ9      3
        HAJ10     3
        HAJ11     3
        HAJ12     3
        HAJ13     3
        HAJ14     3
        HAJ15     3
        HAJ16     3
        HAJ17     3
        HAJ18R    3
        HAK1      3
        HAK2      3
        HAK3A     3
        HAK3A1    3
        HAK3B     3
        HAK3B1R   3
        HAK3C     3
        HAK3C1R   3
        HAK4      3
        HAK5      3
        HAK6      3
        HAK7      3
        HAK8      3
        HAK9      3
        HAK10     3
        HAK11     3
        HAK12     3
        HAK13     3
        HAK14     3
        HAL1      3
        HAL2      3
        HAL3      3
        HAL4      3
        HAL5      3
        HAL6      3
        HAL7      3
        HAL8      3
        HAL9      3
        HAL10     3
        HAL11A    3
        HAL11B    3
        HAL12     3
        HAL13     3
        HAL14A    3
        HAL14B    3
        HAL14C    3
        HAL14D    3
        HAL14E    3
        HAL15A    3
        HAL15B    3
        HAL15C    3
        HAL15D    3
        HAL15E    3
        HAL15F    3
        HAL15G    3
        HAL15H    3
        HAL15I    3
        HAL15J    3
        HAL15K    3
        HAL15L    3
        HAL15M    3
        HAL16A    3
        HAL16B    3
        HAL17     3
        HAL18     3
        HAL19A    3
        HAL19B    3
        HAL19C    3
        HAL20A    3
        HAL20B    3
        HAL20C    3
        HAM1      3
        HAM2      3
        HAM3      3
        HAM4      3
        HAM5S     3
        HAM6S     3
        HAM7      3
        HAM8S     3
        HAM9S     3
        HAM10S    3
        HAM11     3
        HAM12     3
        HAM13     3
        HAM14     3
        HAM15A    3
        HAM15B    3
        HAM15C    3
        HAM15D    3
        HAM15E    3
        HAM15F    3
        HAM15G    3
        HAM15H    3
        HAM15K    3
        HAM15L    3
        HAM15M    3
        HAM15N    3
        HAM15O    3
        HAM15P    3
        HAM15Q    3
        HAM15R    3
        HAM15S    3
        HAM15T    3
        HAM15U    3
        HAM15V    3
        HAM15W    3
        HAM15X    3
        HAM15Y    3
        HAM15Z    3
        HAN1AS    3
        HAN1BS    4
        HAN1C     3
        HAN1D     3
        HAN1ES    3
        HAN1FS    3
        HAN1GS    3
        HAN1HS    3
        HAN1IS    3
        HAN2AS    3
        HAN2BS    3
        HAN2CS    4
        HAN2DS    3
        HAN2ES    3
        HAN2FS    3
        HAN2GS    3
        HAN2HS    3
        HAN2IS    3
        HAN2JS    3
        HAN3AS    3
        HAN3BS    3
        HAN3CS    3
        HAN3DS    3
        HAN3ES    3
        HAN3FS    3
        HAN4AS    3
        HAN4BS    3
        HAN4CS    3
        HAN4DS    3
        HAN4ES    3
        HAN4FS    3
        HAN4GS    3
        HAN4HS    3
        HAN4IS    3
        HAN4JS    3
        HAN4KS    3
        HAN4LS    3
        HAN5AS    3
        HAN5BS    3
        HAN5CS    3
        HAN5DS    3
        HAN5ES    3
        HAN5FS    3
        HAN5GS    3
        HAN5HS    3
        HAN5IS    3
        HAN5JS    3
        HAN5KS    3
        HAN5LS    3
        HAN6AS    4
        HAN6BS    3
        HAN6CS    4
        HAN6DS    3
        HAN6ES    4
        HAN6FS    4
        HAN6GS    4
        HAN6HS    3
        HAN6IS    3
        HAN6JS    3
        HAN7AS    3
        HAN7BS    3
        HAN7CS    3
        HAN8      3
        HAN8ACD   3
        HAN8AS    4
        HAN8BCD   3
        HAN8BS    4
        HAN8CCD   3
        HAN8CS    3
        HAN8DCD   3
        HAN8DS    3
        HAN8ECD   3
        HAN8ES    3
        HAN8FCD   3
        HAN8FS    3
        HAN9      3
        HAN10A    3
        HAN10B    3
        HAN10C    3
        HAN10D    3
        HAN10E    3
        HAP1      3
        HAP1A     3
        HAP2      3
        HAP3      3
        HAP4      3
        HAP5      3
        HAP6      3
        HAP7      3
        HAP8      3
        HAP9      3
        HAP10     3
        HAP10A    3
        HAP11     3
        HAP12     3
        HAP13     3
        HAP14     3
        HAP15     3
        HAP16     3
        HAP17A1   3
        HAP17A2   3
        HAP17A3   3
        HAP17B    3
        HAP18A    3
        HAP18B    3
        HAP18C    3
        HAP18D    3
        HAP18E    3
        HAP19A    3
        HAP19B    3
        HAP19C    3
        HAQ1      3
        HAQ2A     3
        HAQ2B     3
        HAQ2C     3
        HAQ2D     3
        HAQ2E     3
        HAQ2F     3
        HAQ2G     3
        HAQ2I     3
        HAQ2K     3
        HAQ2L     3
        HAQ3S     4
        HAQ4      3
        HAQ5      3
        HAQ6      3
        HAQ7      3
        HAQ8      3
        HAQ9      3
        HAR1      3
        HAR2      3
        HAR3      3
        HAR4S     3
        HAR5      3
        HAR6      3
        HAR7S     4
        HAR8      3
        HAR9      3
        HAR10     3
        HAR11R    3
        HAR12S    3
        HAR13     3
        HAR14     3
        HAR15     3
        HAR16     3
        HAR17     3
        HAR18AS   3
        HAR18BS   3
        HAR19A    3
        HAR19B    3
        HAR20R    3
        HAR21     3
        HAR22     3
        HAR23     3
        HAR24     3
        HAR25     3
        HAR26     3
        HAR27     3
        HAR28     3
        HAS1      3
        HAS2      3
        HAS3      3
        HAS4      3
        HAS5      3
        HAS8R     3
        HAS9R     3
        HAS11     3
        HAS12S    3
        HAS13     3
        HAS14     3
        HAS15     3
        HAS16A    3
        HAS16B    3
        HAS16C    3
        HAS16D    3
        HAS17     3
        HAS17R    3
        HAS18     3
        HAS19     3
        HAS19R    3
        HAS21     3
        HAS22     3
        HAS23     3
        HAS24     3
        HAS25     3
        HAS26     3
        HAS27A    3
        HAS27B    3
        HAS27C    3
        HAS27D    3
        HAS27E    3
        HAS27F    3
        HAS27G    3
        HAS27H    3
        HAS27HCD  3
        HAS28     3
        HAS29     3
        HAT1S     3
        HAT1MET   8
        HAT2      3
        HAT2MET   3
        HAT3S     3
        HAT4      3
        HAT4MET   3
        HAT5S     3
        HAT6      3
        HAT6MET   3
        HAT7S     3
        HAT8      3
        HAT8MET   3
        HAT9S     3
        HAT10     3
        HAT10MET  8
        HAT11S    3
        HAT12     3
        HAT12MET  8
        HAT13S    3
        HAT14     3
        HAT14MET  3
        HAT15S    3
        HAT16     3
        HAT16MET  3
        HAT17S    3
        HAT18     3
        HAT19CD   4
        HAT19MET  8
        HAT20S    3
        HAT21CD   4
        HAT21MET  8
        HAT22S    3
        HAT23CD   4
        HAT23MET  8
        HAT24S    3
        HAT25CD   4
        HAT25MET  8
        HAT26S    3
        HAT27     3
        HAT28     3
        HAT29     3
        HAT30     3
        HAV1S     3
        HAV2S     4
        HAV3S     4
        HAV4S     3
        HAV5      3
        HAV6S     3
        HAV7R     3
        HAV8      3
        HAV9      3
        HAV10R    3
        HAV11     3
        HAV12R    3
        HAV13R    3
        HAX1A     3
        HAX1B     3
        HAX1C     3
        HAX2A     3
        HAX2B     3
        HAX2C     3
        HAX3CG    3
        HAX3DG    3
        HAX6SG    4
        HAX8SG    4
        HAX3CH    3
        HAX3DH    3
        HAX6SH    4
        HAX8SH    4
        HAX3CI    3
        HAX3DI    3
        HAX6SI    4
        HAX8SI    4
        HAX3CJ    3
        HAX3DJ    3
        HAX6SJ    4
        HAX8SJ    4
        HAX3CK    3
        HAX3DK    3
        HAX6SK    4
        HAX8SK    4
        HAX3CL    3
        HAX3DL    3
        HAX6SL    4
        HAX8SL    4
        HAX3CM    3
        HAX3DM    3
        HAX6SM    4
        HAX8SM    4
        HAX3CN    3
        HAX3DN    3
        HAX6SN    4
        HAX8SN    4
        HAX3CO    3
        HAX3DO    3
        HAX6SO    4
        HAX8SO    4
        HAX3CP    3
        HAX3DP    3
        HAX6SP    4
        HAX8SP    4
        HAX3CQ    3
        HAX3DQ    3
        HAX6SQ    4
        HAX8SQ    4
        HAX3CR    3
        HAX3DR    3
        HAX6SR    4
        HAX8SR    4
        HAX3CS    3
        HAX3DS    3
        HAX6SS    4
        HAX8SS    4
        HAX3CT    3
        HAX3DT    3
        HAX6ST    4
        HAX8ST    4
        HAX3CU    3
        HAX3DU    3
        HAX6SU    4
        HAX8SU    4
        HAX3CV    3
        HAX3DV    3
        HAX6SV    4
        HAX8SV    4
        HAX3CW    3
        HAX3DW    3
        HAX6SW    4
        HAX8SW    4
        HAX9DG    4
        HAX9EG    4
        HAX9FG    4
        HAX11AG  $6
        HAX12SG   4
        HAX9DH    4
        HAX9EH    4
        HAX9FH    4
        HAX11AH  $6
        HAX12SH   4
        HAX9DI    4
        HAX9EI    4
        HAX9FI    4
        HAX11AI  $6
        HAX12SI   4
        HAX9DJ    4
        HAX9EJ    4
        HAX9FJ    4
        HAX11AJ  $6
        HAX12SJ   4
        HAX9DK    4
        HAX9EK    4
        HAX9FK    4
        HAX11AK  $6
        HAX12SK   4
        HAX9DL    4
        HAX9EL    4
        HAX9FL    4
        HAX11AL  $6
        HAX12SL   4
        HAX9DM    4
        HAX9EM    4
        HAX9FM    4
        HAX11AM  $6
        HAX12SM   4
        HAX9DN    4
        HAX9EN    4
        HAX9FN    4
        HAX11AN  $6
        HAX12SN   4
        HAX9DO    4
        HAX9EO    4
        HAX9FO    4
        HAX11AO  $6
        HAX12SO   4
        HAX9DP    4
        HAX9EP    4
        HAX9FP    4
        HAX11AP  $6
        HAX12SP   4
        HAX9DQ    4
        HAX9EQ    4
        HAX9FQ    4
        HAX11AQ  $6
        HAX12SQ   4
        HAX9DR    4
        HAX9ER    4
        HAX9FR    4
        HAX11AR  $6
        HAX12SR   4
        HAX9DS    4
        HAX9ES    4
        HAX9FS    4
        HAX11AS  $6
        HAX12SS   4
        HAX9DT    4
        HAX9ET    4
        HAX9FT    4
        HAX11AT  $6
        HAX12ST   4
        HAX9DU    4
        HAX9EU    4
        HAX9FU    4
        HAX11AU  $6
        HAX12SU   4
        HAX9DV    4
        HAX9EV    4
        HAX9FV    4
        HAX11AV  $6
        HAX12SV   4
        HAX13AG   3
        HAX15SG   3
        HAX16AG   3
        HAX16A1G  3
        HAX17SG   4
        HAX13AH   3
        HAX15SH   3
        HAX16AH   3
        HAX16A1H  3
        HAX17SH   4
        HAX13AI   3
        HAX15SI   3
        HAX16AI   3
        HAX16A1I  3
        HAX17SI   4
        HAX13AJ   3
        HAX15SJ   3
        HAX16AJ   3
        HAX16A1J  3
        HAX17SJ   4
        HAX18A    3
        HAX19AS   3
        HAX18B    3
        HAX19BS   3
        HAX18C    3
        HAX19CS   3
        HAX18D    3
        HAX18DB   3
        HAX19DS   3
        HAX18E    3
        HAX18EB   3
        HAX19ES   3
        HAX18F    3
        HAX18FB   3
        HAX19FS   3
        HAX18G    3
        HAX18GB   3
        HAX19GS   3
        HAY6      3
        HAY7      3
        HAY8      3
        HAY9      3
        HAY10     3
        HAY11A    3
        HAY11B    3
        HAY11C    3
        HAY11D    3
        HAY11E    3
        HAY11F    3
        HAY11G    3
        HAY11H    3
        HAY11I    3
        HAY11J    3
        HAZA1     3
        HAZA1A    3
        HAZA1CC  $30
        HAZA2     3
        HAZA2A1   3
        HAZA2A2   3
        HAZA2A3   3
        HAZA2A4   3
        HAZA3     3
        HAZA4     3
        HAZA4A    3
        HAZA5     3
        HAZA5R    3
        HAZA6     3
        HAZA7A    3
        HAZA7AA   3
        HAZA7B    3
        HAZA7BA   3
        HAZA8AK1  3
        HAZA8AK5  3
        HAZA8A3   3
        HAZA8A4   3
        HAZA8BK1  3
        HAZA8BK5  3
        HAZA8B3   3
        HAZA8B4   3
        HAZA8CK1  3
        HAZA8CK5  3
        HAZA8C3   3
        HAZA8C4   3
        HAZA8DK1  3
        HAZA8DK5  3
        HAZA8D3   3
        HAZA8D4   3
        HAZA9     3
        HAZA10    3
        HAZA11A   3
        HAZA11AR  3
        HAZA11B   3
        HAZA11BR  3
        HAZA12    3
        HAZMNK1R  3
        HAZNOK1R  3
        HAZMNK5R  3
        HAZNOK5R  3
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
        HAT1MET  Z3.2
        HAT10MET Z3.2
        HAT12MET Z3.2
        HAT19MET Z4.2
        HAT21MET Z4.2
        HAT23MET Z4.2
        HAT25MET Z3.2
      ;

     INPUT
        SEQN     1-5
        DMPFSEQ  6-10
        DMPSTAT  11
        DMARETHN 12
        DMARACER 13
        DMAETHNR 14
        HSSEX    15
        HSDOIMO  16-17
        HSAGEIR  18-19
        HSAGEU   20
        HSAITMOR 21-24
        HSFSIZER 25-26
        HSHSIZER 27-28
        DMPCNTYR 29-31
        DMPFIPSR 32-33
        DMPMETRO 34
        DMPCREGN 35
        DMPPIR   36-41
        SDPPHASE 42
        SDPPSU6  43
        SDPSTRA6 44-45
        SDPPSU1  46
        SDPSTRA1 47-48
        SDPPSU2  49
        SDPSTRA2 50-51
        WTPFQX6  52-60
        WTPFEX6  61-69
        WTPFHX6  70-78
        WTPFALG6 79-87
        WTPFCNS6 88-96
        WTPFSD6  97-105
        WTPFMD6  106-114
        WTPFHSD6 115-123
        WTPFHMD6 124-132
        WTPFQX1  133-141
        WTPFEX1  142-150
        WTPFHX1  151-159
        WTPFALG1 160-168
        WTPFCNS1 169-177
        WTPFSD1  178-186
        WTPFMD1  187-195
        WTPFHSD1 196-204
        WTPFHMD1 205-213
        WTPFQX2  214-222
        WTPFEX2  223-231
        WTPFHX2  232-240
        WTPFALG2 241-249
        WTPFCNS2 250-258
        WTPFSD2  259-267
        WTPFMD2  268-276
        WTPFHSD2 277-285
        WTPFHMD2 286-294
        WTPQRP1  295-303
        WTPQRP2  304-312
        WTPQRP3  313-321
        WTPQRP4  322-330
        WTPQRP5  331-339
        WTPQRP6  340-348
        WTPQRP7  349-357
        WTPQRP8  358-366
        WTPQRP9  367-375
        WTPQRP10 376-384
        WTPQRP11 385-393
        WTPQRP12 394-402
        WTPQRP13 403-411
        WTPQRP14 412-420
        WTPQRP15 421-429
        WTPQRP16 430-438
        WTPQRP17 439-447
        WTPQRP18 448-456
        WTPQRP19 457-465
        WTPQRP20 466-474
        WTPQRP21 475-483
        WTPQRP22 484-492
        WTPQRP23 493-501
        WTPQRP24 502-510
        WTPQRP25 511-519
        WTPQRP26 520-528
        WTPQRP27 529-537
        WTPQRP28 538-546
        WTPQRP29 547-555
        WTPQRP30 556-564
        WTPQRP31 565-573
        WTPQRP32 574-582
        WTPQRP33 583-591
        WTPQRP34 592-600
        WTPQRP35 601-609
        WTPQRP36 610-618
        WTPQRP37 619-627
        WTPQRP38 628-636
        WTPQRP39 637-645
        WTPQRP40 646-654
        WTPQRP41 655-663
        WTPQRP42 664-672
        WTPQRP43 673-681
        WTPQRP44 682-690
        WTPQRP45 691-699
        WTPQRP46 700-708
        WTPQRP47 709-717
        WTPQRP48 718-726
        WTPQRP49 727-735
        WTPQRP50 736-744
        WTPQRP51 745-753
        WTPQRP52 754-762
        WTPXRP1  763-771
        WTPXRP2  772-780
        WTPXRP3  781-789
        WTPXRP4  790-798
        WTPXRP5  799-807
        WTPXRP6  808-816
        WTPXRP7  817-825
        WTPXRP8  826-834
        WTPXRP9  835-843
        WTPXRP10 844-852
        WTPXRP11 853-861
        WTPXRP12 862-870
        WTPXRP13 871-879
        WTPXRP14 880-888
        WTPXRP15 889-897
        WTPXRP16 898-906
        WTPXRP17 907-915
        WTPXRP18 916-924
        WTPXRP19 925-933
        WTPXRP20 934-942
        WTPXRP21 943-951
        WTPXRP22 952-960
        WTPXRP23 961-969
        WTPXRP24 970-978
        WTPXRP25 979-987
        WTPXRP26 988-996
        WTPXRP27 997-1005
        WTPXRP28 1006-1014
        WTPXRP29 1015-1023
        WTPXRP30 1024-1032
        WTPXRP31 1033-1041
        WTPXRP32 1042-1050
        WTPXRP33 1051-1059
        WTPXRP34 1060-1068
        WTPXRP35 1069-1077
        WTPXRP36 1078-1086
        WTPXRP37 1087-1095
        WTPXRP38 1096-1104
        WTPXRP39 1105-1113
        WTPXRP40 1114-1122
        WTPXRP41 1123-1131
        WTPXRP42 1132-1140
        WTPXRP43 1141-1149
        WTPXRP44 1150-1158
        WTPXRP45 1159-1167
        WTPXRP46 1168-1176
        WTPXRP47 1177-1185
        WTPXRP48 1186-1194
        WTPXRP49 1195-1203
        WTPXRP50 1204-1212
        WTPXRP51 1213-1221
        WTPXRP52 1222-1230
        HYAITMO  1231
        MXPLANG  1232
        MXPSESSR 1233
        MXPTIMO  1234-1235
        MXPTIDW  1236
        MXPAXTMR 1237-1240
        HXPSESSR 1241
        HXPTIMO  1242-1243
        HXPTIDW  1244
        HXPAXTMR 1245-1248
        HFVERS   1249
        HFINTVR  1250-1253
        HFLANG   1254
        HFA6XCR  1255
        HFA8R    1256-1257
        HFA12    1258-1259
        HFA13    1260
        HFB1     1261
        HFB2     1262
        HFB3     1263
        HFB4     1264
        HFB5     1265
        HFB6     1266
        HFB7     1267
        HFB8     1268
        HFB9     1269
        HFB10    1270
        HFB11    1271
        HFB11A   1272
        HFB11B   1273
        HFB11C   1274
        HFB11D   1275
        HFB11E   1276
        HFB12    1277
        HFB13    1278
        HFB14AB  1279
        HFB14AC  1280
        HFB14AD  1281
        HFB14AE  1282
        HFB14AF  1283
        HFB14AG  1284
        HFB14AI  1285
        HFB14BA  1286
        HFB14BB  1287
        HFB14BC  1288
        HFB14BD  1289
        HFB14BE  1290
        HFB14BF  1291
        HFB14BG  1292
        HFB14BH  1293
        HFB16A   1294-1295
        HFB16B   1296-1297
        HFB17    1298
        HFC1     1299
        HFC2S    1300
        HFC3S    1301
        HFC4S    1302
        HFC5S    1303
        HFC6A    1304
        HFC6A1   1305
        HFC6B    1306
        HFC6B1   1307
        HFC6C    1308
        HFC6C1   1309
        HFC6D    1310
        HFC6D1   1311
        HFC6E    1312
        HFC6E1   1313
        HFC7S    1314
        HFC8S    1315
        HFC9     1316
        HFC10    1317
        HFC11    1318
        HFD1     1319
        HFD2     1320
        HFD3     1321
        HFD4     1322
        HFD5     1323
        HFD9R    1324-1325
        HFD10R   1326-1327
        HFD12    1328-1329
        HFE1     1330
        HFE2     1331-1332
        HFE3     1333-1335
        HFE4     1336
        HFE5     1337
        HFE6     1338
        HFE7     1339
        HFE8A    1340
        HFE8B    1341
        HFE8C    1342
        HFE8D    1343
        HFE8E    1344
        HFE9     1345
        HFE10    1346
        HFE11    1347
        HFE12    1348-1349
        HFE13    1350
        HFE14    1351
        HFE15    1352
        HFE16    1353
        HFE17    1354
        HFE18    1355
        HFF1     1356
        HFF2R    1357-1358
        HFF3A    1359-1361
        HFF3B    1362-1364
        HFF3C    1365-1367
        HFF3D    1368-1370
        HFF3E    1371-1373
        HFF3F    1374-1376
        HFF4     1377
        HFF5     1378-1379
        HFF6A    1380
        HFF6B    1381
        HFF6C    1382
        HFF6D    1383
        HFF7     1384
        HFF8     1385
        HFF9     1386
        HFF10    1387
        HFF11    1388
        HFF12A   1389
        HFF12B   1390
        HFF13A   1391
        HFF13B   1392
        HFF13DA  1393
        HFF13DB  1394-1395
        HFF13E   1396
        HFF14A   1397
        HFF14B   1398
        HFF14D   1399
        HFF15A   1400
        HFF15B   1401
        HFF16A   1402
        HFF16B   1403
        HFF16D   1404
        HFF16E   1405
        HFF17A   1406
        HFF17B   1407
        HFF18    1408
        HFF19R   1409-1410
        HFF20R   1411-1412
        HFHSEQN  1413-1417
        HFHAGER  1418-1420
        HFHSEX   1421
        HFHFIPCR 1422
        HFHEDUCR 1423-1424
        HFHEMPL  1425
        HFHMRST  1426-1427
        HFHMLSV  1428
        HFRELR   1429-1430
        HFAGERR  1431-1433
        HAVERS   1434
        HAINTVR  1435-1438
        HALANG   1439
        HAA1     1440
        HAA2     1441
        HAA2A    1442-1443
        HAA3     1444
        HAA4     1445
        HAA5     1446
        HAA6A    1447
        HAA6B    1448
        HAA6C    1449
        HAA6D    1450
        HAB1     1451
        HAB2     1452
        HAB3     1453
        HAB4     1454-1455
        HAB5     1456-1458
        HAB6S    1459-1462
        HAB7     1463-1464
        HAC1A    1465
        HAC1B    1466
        HAC1C    1467
        HAC1D    1468
        HAC1E    1469
        HAC1F    1470
        HAC1G    1471
        HAC1H    1472
        HAC1I    1473
        HAC1J    1474
        HAC1K    1475
        HAC1L    1476
        HAC1M    1477
        HAC1N    1478
        HAC1O    1479
        HAC2E    1480
        HAC2F    1481
        HAC2H    1482
        HAC2J    1483
        HAC2K    1484
        HAC3AR   1485-1487
        HAC3CR   1488-1490
        HAC3DR   1491-1493
        HAC3ER   1494-1496
        HAC3FR   1497-1499
        HAC3GR   1500-1502
        HAC3HR   1503-1505
        HAC3IR   1506-1508
        HAC3JR   1509-1511
        HAC3KR   1512-1514
        HAC3LR   1515-1517
        HAC3MR   1518-1520
        HAC3NR   1521-1523
        HAC3OR   1524-1526
        HAC3OS   1527-1528
        HAC4A    1529
        HAC4B    1530
        HAC5A1   1531
        HAC5A2   1532
        HAC5A3   1533
        HAC5A4   1534
        HAC5A5   1535
        HAC5A6   1536
        HAC5A7   1537
        HAC5A8   1538
        HAC5A9   1539
        HAC5A10  1540
        HAC5A11  1541
        HAC5A12  1542
        HAC5B1   1543
        HAC5B2   1544
        HAC5B3   1545
        HAC5B4   1546
        HAC5B5   1547
        HAC5B6   1548
        HAC5B7   1549
        HAC5B8   1550
        HAC5B9   1551
        HAC5B10  1552
        HAC5B11  1553
        HAC5B12  1554
        HAC6     1555
        HAC7     1556
        HAC7A    1557
        HAC8     1558-1560
        HAD1     1561
        HAD2     1562
        HAD3     1563
        HAD4     1564
        HAD5R    1565-1567
        HAD6     1568
        HAD7S    1569-1570
        HAD8     1571-1573
        HAD9S    1574-1577
        HAD10    1578
        HAD11AS  1579-1581
        HAD11BS  1582-1584
        HAD12S   1585-1589
        HAD13S   1590-1594
        HAD14    1595
        HAD15    1596
        HAE1     1597
        HAE2     1598
        HAE3     1599
        HAE4A    1600
        HAE4B    1601
        HAE4C    1602
        HAE4D    1603
        HAE4D1   1604
        HAE4D2   1605
        HAE4D3   1606
        HAE4D4   1607
        HAE4D5   1608
        HAE4D6   1609
        HAE5A    1610
        HAE5B    1611
        HAE5C    1612
        HAE5D1   1613
        HAE5D2   1614
        HAE5D3   1615
        HAE5D4   1616
        HAE5D5   1617
        HAE5D6   1618
        HAE6     1619
        HAE7     1620
        HAE8A    1621
        HAE8B    1622
        HAE8C    1623
        HAE8D    1624
        HAE9A    1625
        HAE9B    1626
        HAE9C    1627
        HAE9D    1628
        HAE10    1629
        HAE11A   1630
        HAE11B   1631
        HAE11C   1632
        HAF1     1633
        HAF2     1634
        HAF3     1635
        HAF4     1636
        HAF5     1637
        HAF6     1638
        HAF7A    1639
        HAF7B    1640
        HAF7C    1641
        HAF7D    1642
        HAF7E    1643
        HAF7F    1644
        HAF7G    1645
        HAF7H    1646
        HAF9     1647
        HAF10    1648
        HAF11    1649-1650
        HAF12R   1651-1653
        HAF13R   1654-1656
        HAF14    1657
        HAF15    1658
        HAF16    1659
        HAF17    1660
        HAF18    1661
        HAF19    1662
        HAF20    1663
        HAF21    1664
        HAF22    1665
        HAF23    1666
        HAF24    1667
        HAF25    1668
        HAF26    1669
        HAF27    1670
        HAG1     1671
        HAG2     1672
        HAG3     1673
        HAG4A    1674
        HAG4B    1675
        HAG4C    1676
        HAG5A    1677
        HAG5B    1678
        HAG5C    1679
        HAG6A    1680-1681
        HAG6B    1682-1683
        HAG6C    1684-1685
        HAG7     1686
        HAG8A    1687
        HAG8B    1688
        HAG8C    1689
        HAG8D    1690
        HAG8E    1691
        HAG8F    1692
        HAG9AR   1693-1695
        HAG9BR   1696-1698
        HAG9CR   1699-1701
        HAG9DR   1702-1704
        HAG9ER   1705-1707
        HAG9FR   1708-1710
        HAG10A   1711
        HAG10B   1712
        HAG10C   1713
        HAG10D   1714
        HAG10E   1715
        HAG10F   1716
        HAG11    1717
        HAG12    1718
        HAG13    1719
        HAG14    1720-1721
        HAG15    1722
        HAG16    1723
        HAG17A   1724
        HAG17B   1725
        HAG17C   1726
        HAG17D   1727
        HAG17E   1728
        HAG17F   1729
        HAG17G   1730
        HAG17H   1731
        HAG17I   1732
        HAG17J   1733
        HAG17K   1734
        HAG17L   1735
        HAG17M   1736
        HAG17N   1737
        HAG17O   1738
        HAG17FF  1739
        HAG17P   1740
        HAG17Q   1741
        HAG17R   1742
        HAG17S   1743
        HAG17T   1744
        HAG17U   1745
        HAG17V   1746
        HAG17W   1747
        HAG17X   1748
        HAG17Y   1749
        HAG17Z   1750
        HAG17AA  1751
        HAG17BB  1752
        HAG17CC  1753
        HAG17DD  1754
        HAG17EE  1755
        HAG18    1756
        HAG19A   1757
        HAG19B   1758
        HAG19C   1759
        HAG19D   1760
        HAG19E   1761
        HAG19F   1762
        HAG19G   1763
        HAG19H   1764
        HAG19I   1765
        HAG19J   1766
        HAG19K   1767
        HAG19L   1768
        HAG19M   1769
        HAG19N   1770
        HAG19O   1771
        HAG19FF  1772
        HAG19P   1773
        HAG19Q   1774
        HAG19R   1775
        HAG19S   1776
        HAG19T   1777
        HAG19U   1778
        HAG19V   1779
        HAG19W   1780
        HAG19X   1781
        HAG19Y   1782
        HAG19Z   1783
        HAG19AA  1784
        HAG19BB  1785
        HAG19CC  1786
        HAG19DD  1787
        HAG19EE  1788
        HAG20    1789
        HAG21    1790
        HAG22    1791
        HAG23    1792
        HAG24    1793
        HAG25    1794
        HAG26    1795
        HAG27    1796
        HAG28    1797
        HAG29    1798
        HAG30    1799
        HAG31    1800
        HAG32    1801
        HAG33    1802
        HAG34    1803
        HAH1     1804
        HAH2     1805
        HAH3     1806
        HAH4     1807
        HAH5     1808
        HAH6     1809
        HAH7     1810
        HAH8     1811
        HAH9     1812
        HAH10    1813
        HAH11    1814
        HAH12    1815
        HAH13    1816
        HAH14    1817
        HAH15    1818
        HAH16    1819
        HAH17    1820
        HAJ0     1821
        HAJ1     1822
        HAJ2A    1823
        HAJ2B    1824
        HAJ2C    1825
        HAJ3S    1826-1831
        HAJ4     1832-1834
        HAJ5     1835
        HAJ6     1836
        HAJ7     1837
        HAJ8     1838-1839
        HAJ9     1840
        HAJ10    1841-1842
        HAJ11    1843
        HAJ12    1844
        HAJ13    1845-1847
        HAJ14    1848
        HAJ15    1849
        HAJ16    1850
        HAJ17    1851
        HAJ18R   1852-1854
        HAK1     1855
        HAK2     1856-1857
        HAK3A    1858
        HAK3A1   1859-1861
        HAK3B    1862
        HAK3B1R  1863-1865
        HAK3C    1866
        HAK3C1R  1867-1869
        HAK4     1870
        HAK5     1871-1872
        HAK6     1873
        HAK7     1874
        HAK8     1875-1877
        HAK9     1878
        HAK10    1879
        HAK11    1880
        HAK12    1881
        HAK13    1882
        HAK14    1883
        HAL1     1884
        HAL2     1885-1887
        HAL3     1888
        HAL4     1889-1891
        HAL5     1892
        HAL6     1893
        HAL7     1894-1896
        HAL8     1897-1899
        HAL9     1900-1902
        HAL10    1903
        HAL11A   1904
        HAL11B   1905
        HAL12    1906-1908
        HAL13    1909
        HAL14A   1910
        HAL14B   1911
        HAL14C   1912
        HAL14D   1913
        HAL14E   1914
        HAL15A   1915
        HAL15B   1916
        HAL15C   1917
        HAL15D   1918
        HAL15E   1919
        HAL15F   1920
        HAL15G   1921
        HAL15H   1922
        HAL15I   1923
        HAL15J   1924
        HAL15K   1925
        HAL15L   1926
        HAL15M   1927
        HAL16A   1928
        HAL16B   1929
        HAL17    1930
        HAL18    1931
        HAL19A   1932
        HAL19B   1933
        HAL19C   1934
        HAL20A   1935-1937
        HAL20B   1938-1940
        HAL20C   1941-1943
        HAM1     1944
        HAM2     1945
        HAM3     1946
        HAM4     1947
        HAM5S    1948-1950
        HAM6S    1951-1953
        HAM7     1954
        HAM8S    1955-1957
        HAM9S    1958-1960
        HAM10S   1961-1963
        HAM11    1964
        HAM12    1965
        HAM13    1966
        HAM14    1967
        HAM15A   1968
        HAM15B   1969
        HAM15C   1970
        HAM15D   1971
        HAM15E   1972
        HAM15F   1973
        HAM15G   1974
        HAM15H   1975
        HAM15K   1976
        HAM15L   1977
        HAM15M   1978
        HAM15N   1979
        HAM15O   1980
        HAM15P   1981
        HAM15Q   1982
        HAM15R   1983
        HAM15S   1984
        HAM15T   1985
        HAM15U   1986
        HAM15V   1987
        HAM15W   1988
        HAM15X   1989
        HAM15Y   1990
        HAM15Z   1991
        HAN1AS   1992-1994
        HAN1BS   1995-1998
        HAN1C    1999
        HAN1D    2000-2001
        HAN1ES   2002-2004
        HAN1FS   2005-2007
        HAN1GS   2008-2010
        HAN1HS   2011-2013
        HAN1IS   2014-2016
        HAN2AS   2017-2019
        HAN2BS   2020-2022
        HAN2CS   2023-2026
        HAN2DS   2027-2029
        HAN2ES   2030-2032
        HAN2FS   2033-2035
        HAN2GS   2036-2038
        HAN2HS   2039-2041
        HAN2IS   2042-2044
        HAN2JS   2045-2047
        HAN3AS   2048-2050
        HAN3BS   2051-2053
        HAN3CS   2054-2056
        HAN3DS   2057-2059
        HAN3ES   2060-2062
        HAN3FS   2063-2065
        HAN4AS   2066-2068
        HAN4BS   2069-2071
        HAN4CS   2072-2074
        HAN4DS   2075-2077
        HAN4ES   2078-2080
        HAN4FS   2081-2083
        HAN4GS   2084-2086
        HAN4HS   2087-2089
        HAN4IS   2090-2092
        HAN4JS   2093-2095
        HAN4KS   2096-2098
        HAN4LS   2099-2101
        HAN5AS   2102-2104
        HAN5BS   2105-2107
        HAN5CS   2108-2110
        HAN5DS   2111-2113
        HAN5ES   2114-2116
        HAN5FS   2117-2119
        HAN5GS   2120-2122
        HAN5HS   2123-2125
        HAN5IS   2126-2128
        HAN5JS   2129-2131
        HAN5KS   2132-2134
        HAN5LS   2135-2137
        HAN6AS   2138-2141
        HAN6BS   2142-2144
        HAN6CS   2145-2148
        HAN6DS   2149-2151
        HAN6ES   2152-2155
        HAN6FS   2156-2159
        HAN6GS   2160-2163
        HAN6HS   2164-2166
        HAN6IS   2167-2169
        HAN6JS   2170-2172
        HAN7AS   2173-2175
        HAN7BS   2176-2178
        HAN7CS   2179-2181
        HAN8     2182
        HAN8ACD  2183-2184
        HAN8AS   2185-2188
        HAN8BCD  2189-2190
        HAN8BS   2191-2194
        HAN8CCD  2195-2196
        HAN8CS   2197-2199
        HAN8DCD  2200-2201
        HAN8DS   2202-2204
        HAN8ECD  2205-2206
        HAN8ES   2207-2209
        HAN8FCD  2210-2211
        HAN8FS   2212-2214
        HAN9     2215
        HAN10A   2216
        HAN10B   2217
        HAN10C   2218
        HAN10D   2219
        HAN10E   2220
        HAP1     2221
        HAP1A    2222
        HAP2     2223
        HAP3     2224
        HAP4     2225
        HAP5     2226
        HAP6     2227
        HAP7     2228
        HAP8     2229
        HAP9     2230
        HAP10    2231
        HAP10A   2232
        HAP11    2233
        HAP12    2234
        HAP13    2235
        HAP14    2236
        HAP15    2237
        HAP16    2238
        HAP17A1  2239
        HAP17A2  2240
        HAP17A3  2241
        HAP17B   2242
        HAP18A   2243-2244
        HAP18B   2245-2246
        HAP18C   2247-2248
        HAP18D   2249-2250
        HAP18E   2251-2252
        HAP19A   2253
        HAP19B   2254
        HAP19C   2255
        HAQ1     2256-2257
        HAQ2A    2258
        HAQ2B    2259
        HAQ2C    2260
        HAQ2D    2261
        HAQ2E    2262
        HAQ2F    2263
        HAQ2G    2264
        HAQ2I    2265
        HAQ2K    2266
        HAQ2L    2267
        HAQ3S    2268-2272
        HAQ4     2273-2274
        HAQ5     2275
        HAQ6     2276
        HAQ7     2277
        HAQ8     2278
        HAQ9     2279-2280
        HAR1     2281
        HAR2     2282-2284
        HAR3     2285
        HAR4S    2286-2288
        HAR5     2289-2291
        HAR6     2292
        HAR7S    2293-2296
        HAR8     2297-2299
        HAR9     2300
        HAR10    2301-2303
        HAR11R   2304-2306
        HAR12S   2307-2309
        HAR13    2310
        HAR14    2311
        HAR15    2312-2314
        HAR16    2315
        HAR17    2316
        HAR18AS  2317-2319
        HAR18BS  2320-2322
        HAR19A   2323
        HAR19B   2324
        HAR20R   2325-2327
        HAR21    2328
        HAR22    2329
        HAR23    2330
        HAR24    2331
        HAR25    2332-2333
        HAR26    2334
        HAR27    2335
        HAR28    2336-2337
        HAS1     2338
        HAS2     2339
        HAS3     2340
        HAS4     2341
        HAS5     2342
        HAS8R    2343-2344
        HAS9R    2345-2346
        HAS11    2347-2348
        HAS12S   2349-2352
        HAS13    2353
        HAS14    2354-2355
        HAS15    2356
        HAS16A   2357
        HAS16B   2358
        HAS16C   2359
        HAS16D   2360
        HAS17    2361
        HAS17R   2362-2363
        HAS18    2364-2366
        HAS19    2367
        HAS19R   2368-2369
        HAS21    2370
        HAS22    2371-2372
        HAS23    2373
        HAS24    2374
        HAS25    2375
        HAS26    2376
        HAS27A   2377
        HAS27B   2378
        HAS27C   2379
        HAS27D   2380
        HAS27E   2381
        HAS27F   2382
        HAS27G   2383
        HAS27H   2384
        HAS27HCD 2385
        HAS28    2386
        HAS29    2387-2388
        HAT1S    2389-2392
        HAT1MET  2393-2395
        HAT2     2396
        HAT2MET  2397
        HAT3S    2398-2401
        HAT4     2402
        HAT4MET  2403
        HAT5S    2404-2407
        HAT6     2408
        HAT6MET  2409
        HAT7S    2410-2413
        HAT8     2414
        HAT8MET  2415
        HAT9S    2416-2419
        HAT10    2420
        HAT10MET 2421-2423
        HAT11S   2424-2427
        HAT12    2428
        HAT12MET 2429-2431
        HAT13S   2432-2435
        HAT14    2436
        HAT14MET 2437
        HAT15S   2438-2441
        HAT16    2442
        HAT16MET 2443
        HAT17S   2444-2447
        HAT18    2448
        HAT19CD  2449-2453
        HAT19MET 2454-2457
        HAT20S   2458-2461
        HAT21CD  2462-2466
        HAT21MET 2467-2469
        HAT22S   2470-2473
        HAT23CD  2474-2478
        HAT23MET 2479-2481
        HAT24S   2482-2485
        HAT25CD  2486-2490
        HAT25MET 2491-2493
        HAT26S   2494-2497
        HAT27    2498
        HAT28    2499
        HAT29    2500
        HAT30    2501
        HAV1S    2502-2505
        HAV2S    2506-2510
        HAV3S    2511-2515
        HAV4S    2516-2519
        HAV5     2520
        HAV6S    2521-2524
        HAV7R    2525-2528
        HAV8     2529-2530
        HAV9     2531
        HAV10R   2532
        HAV11    2533-2534
        HAV12R   2535
        HAV13R   2536-2538
        HAX1A    2539
        HAX1B    2540
        HAX1C    2541
        HAX2A    2542-2543
        HAX2B    2544-2545
        HAX2C    2546-2547
        HAX3CG   2548-2549
        HAX3DG   2550-2551
        HAX6SG   2552-2555
        HAX8SG   2556-2559
        HAX3CH   2560-2561
        HAX3DH   2562-2563
        HAX6SH   2564-2567
        HAX8SH   2568-2571
        HAX3CI   2572-2573
        HAX3DI   2574-2575
        HAX6SI   2576-2579
        HAX8SI   2580-2583
        HAX3CJ   2584-2585
        HAX3DJ   2586-2587
        HAX6SJ   2588-2591
        HAX8SJ   2592-2595
        HAX3CK   2596-2597
        HAX3DK   2598-2599
        HAX6SK   2600-2603
        HAX8SK   2604-2607
        HAX3CL   2608-2609
        HAX3DL   2610-2611
        HAX6SL   2612-2615
        HAX8SL   2616-2619
        HAX3CM   2620-2621
        HAX3DM   2622-2623
        HAX6SM   2624-2627
        HAX8SM   2628-2631
        HAX3CN   2632-2633
        HAX3DN   2634-2635
        HAX6SN   2636-2639
        HAX8SN   2640-2643
        HAX3CO   2644-2645
        HAX3DO   2646-2647
        HAX6SO   2648-2651
        HAX8SO   2652-2655
        HAX3CP   2656-2657
        HAX3DP   2658-2659
        HAX6SP   2660-2663
        HAX8SP   2664-2667
        HAX3CQ   2668-2669
        HAX3DQ   2670-2671
        HAX6SQ   2672-2675
        HAX8SQ   2676-2679
        HAX3CR   2680-2681
        HAX3DR   2682-2683
        HAX6SR   2684-2687
        HAX8SR   2688-2691
        HAX3CS   2692-2693
        HAX3DS   2694-2695
        HAX6SS   2696-2699
        HAX8SS   2700-2703
        HAX3CT   2704-2705
        HAX3DT   2706-2707
        HAX6ST   2708-2711
        HAX8ST   2712-2715
        HAX3CU   2716-2717
        HAX3DU   2718-2719
        HAX6SU   2720-2723
        HAX8SU   2724-2727
        HAX3CV   2728-2729
        HAX3DV   2730-2731
        HAX6SV   2732-2735
        HAX8SV   2736-2739
        HAX3CW   2740-2741
        HAX3DW   2742-2743
        HAX6SW   2744-2747
        HAX8SW   2748-2751
        HAX9DG   2752-2755
        HAX9EG   2756-2759
        HAX9FG   2760-2763
        HAX11AG  2764-2769
        HAX12SG  2770-2774
        HAX9DH   2775-2778
        HAX9EH   2779-2782
        HAX9FH   2783-2786
        HAX11AH  2787-2792
        HAX12SH  2793-2797
        HAX9DI   2798-2801
        HAX9EI   2802-2805
        HAX9FI   2806-2809
        HAX11AI  2810-2815
        HAX12SI  2816-2820
        HAX9DJ   2821-2824
        HAX9EJ   2825-2828
        HAX9FJ   2829-2832
        HAX11AJ  2833-2838
        HAX12SJ  2839-2843
        HAX9DK   2844-2847
        HAX9EK   2848-2851
        HAX9FK   2852-2855
        HAX11AK  2856-2861
        HAX12SK  2862-2866
        HAX9DL   2867-2870
        HAX9EL   2871-2874
        HAX9FL   2875-2878
        HAX11AL  2879-2884
        HAX12SL  2885-2889
        HAX9DM   2890-2893
        HAX9EM   2894-2897
        HAX9FM   2898-2901
        HAX11AM  2902-2907
        HAX12SM  2908-2912
        HAX9DN   2913-2916
        HAX9EN   2917-2920
        HAX9FN   2921-2924
        HAX11AN  2925-2930
        HAX12SN  2931-2935
        HAX9DO   2936-2939
        HAX9EO   2940-2943
        HAX9FO   2944-2947
        HAX11AO  2948-2953
        HAX12SO  2954-2958
        HAX9DP   2959-2962
        HAX9EP   2963-2966
        HAX9FP   2967-2970
        HAX11AP  2971-2976
        HAX12SP  2977-2981
        HAX9DQ   2982-2985
        HAX9EQ   2986-2989
        HAX9FQ   2990-2993
        HAX11AQ  2994-2999
        HAX12SQ  3000-3004
        HAX9DR   3005-3008
        HAX9ER   3009-3012
        HAX9FR   3013-3016
        HAX11AR  3017-3022
        HAX12SR  3023-3027
        HAX9DS   3028-3031
        HAX9ES   3032-3035
        HAX9FS   3036-3039
        HAX11AS  3040-3045
        HAX12SS  3046-3050
        HAX9DT   3051-3054
        HAX9ET   3055-3058
        HAX9FT   3059-3062
        HAX11AT  3063-3068
        HAX12ST  3069-3073
        HAX9DU   3074-3077
        HAX9EU   3078-3081
        HAX9FU   3082-3085
        HAX11AU  3086-3091
        HAX12SU  3092-3096
        HAX9DV   3097-3100
        HAX9EV   3101-3104
        HAX9FV   3105-3108
        HAX11AV  3109-3114
        HAX12SV  3115-3119
        HAX13AG  3120-3122
        HAX15SG  3123-3126
        HAX16AG  3127-3128
        HAX16A1G 3129-3130
        HAX17SG  3131-3134
        HAX13AH  3135-3137
        HAX15SH  3138-3141
        HAX16AH  3142-3143
        HAX16A1H 3144-3145
        HAX17SH  3146-3149
        HAX13AI  3150-3152
        HAX15SI  3153-3156
        HAX16AI  3157-3158
        HAX16A1I 3159-3160
        HAX17SI  3161-3164
        HAX13AJ  3165-3167
        HAX15SJ  3168-3171
        HAX16AJ  3172-3173
        HAX16A1J 3174-3175
        HAX17SJ  3176-3179
        HAX18A   3180
        HAX19AS  3181-3183
        HAX18B   3184
        HAX19BS  3185-3187
        HAX18C   3188
        HAX19CS  3189-3191
        HAX18D   3192
        HAX18DB  3193-3194
        HAX19DS  3195-3197
        HAX18E   3198
        HAX18EB  3199-3200
        HAX19ES  3201-3203
        HAX18F   3204
        HAX18FB  3205-3206
        HAX19FS  3207-3209
        HAX18G   3210
        HAX18GB  3211-3212
        HAX19GS  3213-3215
        HAY6     3216-3217
        HAY7     3218-3219
        HAY8     3220
        HAY9     3221
        HAY10    3222
        HAY11A   3223
        HAY11B   3224
        HAY11C   3225
        HAY11D   3226
        HAY11E   3227
        HAY11F   3228
        HAY11G   3229
        HAY11H   3230
        HAY11I   3231
        HAY11J   3232
        HAZA1    3233
        HAZA1A   3234
        HAZA1CC  3235-3264
        HAZA2    3265
        HAZA2A1  3266
        HAZA2A2  3267
        HAZA2A3  3268
        HAZA2A4  3269
        HAZA3    3270
        HAZA4    3271
        HAZA4A   3272
        HAZA5    3273-3275
        HAZA5R   3276-3278
        HAZA6    3279
        HAZA7A   3280-3282
        HAZA7AA  3283
        HAZA7B   3284-3286
        HAZA7BA  3287
        HAZA8AK1 3288-3290
        HAZA8AK5 3291-3293
        HAZA8A3  3294
        HAZA8A4  3295
        HAZA8BK1 3296-3298
        HAZA8BK5 3299-3301
        HAZA8B3  3302
        HAZA8B4  3303
        HAZA8CK1 3304-3306
        HAZA8CK5 3307-3309
        HAZA8C3  3310
        HAZA8C4  3311
        HAZA8DK1 3312-3314
        HAZA8DK5 3315-3317
        HAZA8D3  3318
        HAZA8D4  3319
        HAZA9    3320-3321
        HAZA10   3322-3323
        HAZA11A  3324-3326
        HAZA11AR 3327-3329
        HAZA11B  3330-3332
        HAZA11BR 3333-3335
        HAZA12   3336
        HAZMNK1R 3337-3339
        HAZNOK1R 3340-3341
        HAZMNK5R 3342-3344
        HAZNOK5R 3345-3346
     ;

      LABEL
        SEQN     = "Sequence number"
        DMPFSEQ  = "Family sequence number"
        DMPSTAT  = "Examination/interview status"
        DMARETHN = "Race-ethnicity"
        DMARACER = "Race"
        DMAETHNR = "Ethnicity"
        HSSEX    = "Sex"
        HSDOIMO  = "Date of screener: month"
        HSAGEIR  = "Age at interview (screener) - qty"
        HSAGEU   = "Age at interview (screener) - unit"
        HSAITMOR = "Age in months at interview (screener)"
        HSFSIZER = "Family size (persons in family)"
        HSHSIZER = "Household size (persons in dwelling)"
        DMPCNTYR = "County code"
        DMPFIPSR = "FIPS code for State"
        DMPMETRO = "Rural/urban code based on USDA code"
        DMPCREGN = "Census region, weighting(Texas in south)"
        DMPPIR   = "Poverty Income Ratio (unimputed income)"
        SDPPHASE = "Phase of NHANES III Survey"
        SDPPSU6  = "Total NHANES III pseudo-PSU"
        SDPSTRA6 = "Total NHANES III pseudo-stratum"
        SDPPSU1  = "Pseudo-PSU for phase 1"
        SDPSTRA1 = "Pseudo-stratum for phase 1"
        SDPPSU2  = "Pseudo-PSU for phase 2"
        SDPSTRA2 = "Pseudo-stratum for phase 2"
        WTPFQX6  = "Total interviewed sample final weight"
        WTPFEX6  = "Total MEC examined sample final weight"
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
        MXPTIMO  = "Date of MEC exam time in: month"
        MXPTIDW  = "Day of week of MEC exam"
        MXPAXTMR = "Age in months at MEC exam"
        HXPSESSR = "Session for home examination"
        HXPTIMO  = "Date of home exam: month"
        HXPTIDW  = "Day of week of home exam"
        HXPAXTMR = "Age in months at home exam"
        HFVERS   = "Version of household family quex"
        HFINTVR  = "Family interviewer number"
        HFLANG   = "Language of family interview"
        HFA6XCR  = "State or foreign country of birth"
        HFA8R    = "Highest grade or yr of school completed"
        HFA12    = "Marital status"
        HFA13    = "Military service"
        HFB1     = "Any SP covered by Medicare in last month"
        HFB2     = "Covered by Medicare in last month"
        HFB3     = "Type of Medicare coverage"
        HFB4     = "Covered by Medicare last mo for hospital"
        HFB5     = "Covered by Medicare last mo for doctor"
        HFB6     = "Any SP covered by Medicaid in last month"
        HFB7     = "Covered by Medicaid in last month"
        HFB8     = "Any SP covered by CHAMPUS, etc, last mo"
        HFB9     = "Covered by CHAMPUS/VA/military last mo"
        HFB10    = "Any SP cov by other health insur last mo"
        HFB11    = "Covered by other health insur last month"
        HFB11A   = "Any plan cover any hospital expenses"
        HFB11B   = "Any plan cover routine doctor's care"
        HFB11C   = "Any plan cover dental care (phase 2)"
        HFB11D   = "Any plan from employer or union (phase2)"
        HFB11E   = "Covered plan pays for operations (ph 1)"
        HFB12    = "Refused/limited insur, poor health (ph2)"
        HFB13    = "Insurance coverage check item"
        HFB14AB  = "No insurance: unemployed or related(ph1)"
        HFB14AC  = "No insur: poor health, illness, age(ph1)"
        HFB14AD  = "No insurance: can't afford (phase 1)"
        HFB14AE  = "No insur: dissatisfied w/ previous (ph1)"
        HFB14AF  = "No insurance: don't believe in (phase 1)"
        HFB14AG  = "No insurance: healthy, no need (phase 1)"
        HFB14AI  = "No insurance: some other reason(phase 1)"
        HFB14BA  = "No insur: refused for health reason(ph2)"
        HFB14BB  = "No insur: not employed,can't afford(ph2)"
        HFB14BC  = "No insur: not employed,not continue(ph2)"
        HFB14BD  = "No insur: employer does not offer (ph 2)"
        HFB14BE  = "No insur:emplyr offers,not eligible(ph2)"
        HFB14BF  = "No insur:emplyr offers,can't afford(ph2)"
        HFB14BG  = "No insur:no believe/not get/no need(ph2)"
        HFB14BH  = "No insurance:some other reason (phase 2)"
        HFB16A   = "Main reason not covered (phase 1)"
        HFB16B   = "Main reason not covered (phase 2)"
        HFB17    = "Insurance that pays any dental (phase 1)"
        HFC1     = "Age 17+ years check item"
        HFC2S    = "Mother's weight calculated in pounds"
        HFC3S    = "Mother's height calculated in inches"
        HFC4S    = "Father's weight calculated in pounds"
        HFC5S    = "Father's height calculated in inches"
        HFC6A    = "Parent high blood pres/stroke before 50"
        HFC6A1   = "Which parent, high blood pres/stroke"
        HFC6B    = "Parent heart attack/angina before 50"
        HFC6B1   = "Which parent, heart attack/angina"
        HFC6C    = "Parent high blood cholesterol at any age"
        HFC6C1   = "Which parent, high blood cholesterol"
        HFC6D    = "Parent asthma or hay fever at any age"
        HFC6D1   = "Which parent, asthma or hay fever"
        HFC6E    = "Parent diabetes at any age"
        HFC6E1   = "Which parent, diabetes"
        HFC7S    = "How long at address calculated in weeks"
        HFC8S    = "How long in city/town/area calc in weeks"
        HFC9     = "How many times moved"
        HFC10    = "Country mother born"
        HFC11    = "Country father born"
        HFD1     = "Head of family check item"
        HFD2     = "Work at job during past 2 weeks"
        HFD3     = "Had job/business during last 2 weeks"
        HFD4     = "Looking for work or on layoff"
        HFD5     = "Which, looking for work or on layoff"
        HFD9R    = "What kind of business or industry"
        HFD10R   = "What kind of work was person doing"
        HFD12    = "Class of worker"
        HFE1     = "When house/structure originally built"
        HFE2     = "How many rooms are in this home"
        HFE3     = "How long family lived at address - years"
        HFE4     = "Is home drinking water bottled or tap"
        HFE5     = "What is source of your home tap water"
        HFE6     = "Water softening or conditioning system"
        HFE7     = "Do any pets live in this home"
        HFE8A    = "Pet lives here - a dog"
        HFE8B    = "Pet lives here - a cat"
        HFE8C    = "Pet lives here - a bird"
        HFE8D    = "Pet lives here - a fish"
        HFE8E    = "Pet lives here - other pet"
        HFE9     = "Was forced air heat used in past 12 mo"
        HFE10    = "Furnace fueled by oil, gas, elect, other"
        HFE11    = "Was space heater used in past 12 months"
        HFE12    = "Space htr fueled by elect, oil,gas,other"
        HFE13    = "Was wood stove used in past 12 months"
        HFE14    = "Was fireplace used in past 12 months"
        HFE15    = "Is gas stove or oven used for cooking"
        HFE16    = "Is exhaust fan near this stove"
        HFE17    = "How often is exhaust fan used"
        HFE18    = "Was stove/oven used to heat, past 12 mo"
        HFF1     = "Anyone living here smoke cigs in home"
        HFF2R    = "# persons who smoke cigarettes in home"
        HFF3A    = "Cigarettes per day person 1 smokes"
        HFF3B    = "Cigarettes per day person 2 smokes"
        HFF3C    = "Cigarettes per day person 3 smokes"
        HFF3D    = "Cigarettes per day person 4 smokes"
        HFF3E    = "Cigarettes per day person 5 smokes"
        HFF3F    = "Cigarettes per day person 6 smokes"
        HFF4     = "Do you have enough food to eat"
        HFF5     = "# days had no food/money in past month"
        HFF6A    = "Reason no food: transportation problems"
        HFF6B    = "Reason no food: no working appliances"
        HFF6C    = "Reason no food: no money/food stamps/WIC"
        HFF6D    = "Reason no food: other reason"
        HFF7     = "Cut meal size, no money/food, past month"
        HFF8     = "Cut child's meal size, no money, past mo"
        HFF9     = "Any receive benefits from WIC in last mo"
        HFF10    = "Any receive food stamps in last 12 month"
        HFF11    = "Any receive food stamps at present time"
        HFF12A   = "Any receive employment income,last month"
        HFF12B   = "Receive employment income in last month"
        HFF13A   = "Anyone receive SS/RR pay in last month"
        HFF13B   = "Receive SS/RR pay in last month"
        HFF13DA  = "Reason getting SS/RR (phase 1)"
        HFF13DB  = "Reason getting SS/RR (phase 2)"
        HFF13E   = "Another reason getting SS (phase 1)"
        HFF14A   = "Any receive SSI pay, Federal, last month"
        HFF14B   = "Receive SSI pay, Federal, in last month"
        HFF14D   = "Recv separate SSI,welfare,past 4 mo(ph1)"
        HFF15A   = "Any receive other pension/disabl,last mo"
        HFF15B   = "Receive other pension/disability,last mo"
        HFF16A   = "Any receive public asst/welfare, last mo"
        HFF16B   = "Receive public assist/welfare, last mo"
        HFF16D   = "Anyone receive AFDC/ADC in last month"
        HFF16E   = "Receive AFDC/ADC in last month"
        HFF17A   = "Anyone receive other income, last month"
        HFF17B   = "Receive other income in last month"
        HFF18    = "Total family 12 month income < $20,000"
        HFF19R   = "Total family 12 month income group (ph1)"
        HFF20R   = "Total family income, last mo, group(ph1)"
        HFHSEQN  = "Family reference person sequence number"
        HFHAGER  = "Family reference person age - years"
        HFHSEX   = "Family reference person sex"
        HFHFIPCR = "Family reference person birthplace"
        HFHEDUCR = "Family refer person high grade completed"
        HFHEMPL  = "Family refer person employed, past 2 wks"
        HFHMRST  = "Family reference person marital status"
        HFHMLSV  = "Family reference person military service"
        HFRELR   = "Respondent relationship to refer person"
        HFAGERR  = "Respondent age in years"
        HAVERS   = "Phase and questionnaire version"
        HAINTVR  = "Interviewer number"
        HALANG   = "Language of interview"
        HAA1     = "Did SP receive introductory letter"
        HAA2     = "Did SP hear about survey any other way"
        HAA2A    = "Other way heard about the survey"
        HAA3     = "Check Item. Refer to age of  SP"
        HAA4     = "What is today's date"
        HAA5     = "What is the day of the week"
        HAA6A    = "What is your complete address: street"
        HAA6B    = "What is your complete address: city/town"
        HAA6C    = "What is your complete address: state"
        HAA6D    = "What is your complete address: zip code"
        HAB1     = "Is health in general excellent,...,poor"
        HAB2     = "Go to particular place for health care"
        HAB3     = "Is there one particular doctor you see"
        HAB4     = "Past 12 mos, # times stayed in hospital"
        HAB5     = "Past 12 months, # times saw doctor"
        HAB6S    = "How long since saw doctor, etc - months"
        HAB7     = "Past 12 mos, # times in nursing home"
        HAC1A    = "Doctor ever told you had: arthritis"
        HAC1B    = "Type arthritis:rheumatoid,osteoarthritis"
        HAC1C    = "Doctor told: congestive heart failure"
        HAC1D    = "Doctor ever told you had: stroke"
        HAC1E    = "Doctor ever told you had: asthma"
        HAC1F    = "Doctor ever told had: chronic bronchitis"
        HAC1G    = "Doctor ever told you had: emphysema"
        HAC1H    = "Doctor ever told you had: hay fever"
        HAC1I    = "Doctor ever told you had: cataracts"
        HAC1J    = "Doctor ever told you had: goiter"
        HAC1K    = "Doctor ever told you had:thyroid disease"
        HAC1L    = "Doctor ever told you had: lupus"
        HAC1M    = "Doctor ever told you had: gout"
        HAC1N    = "Doctor ever told you had: skin cancer"
        HAC1O    = "Doctor ever told you had: other cancer"
        HAC2E    = "Do you still have asthma"
        HAC2F    = "Do you still have bronchitis"
        HAC2H    = "Do you still have hay fever"
        HAC2J    = "Do you still have goiter"
        HAC2K    = "Do you still have thyroid disease"
        HAC3AR   = "Age when first told you had arth -- yrs"
        HAC3CR   = "Age 1st told had cong heart fail -- yrs"
        HAC3DR   = "Age when 1st told you had stroke -- yrs"
        HAC3ER   = "Age when 1st told you had asthma -- yrs"
        HAC3FR   = "Age when 1st told you had bronch -- yrs"
        HAC3GR   = "Age when first told you had emph -- yrs"
        HAC3HR   = "Age when 1st told you had hay fev -- yrs"
        HAC3IR   = "Age when 1st told you had catar -- yrs"
        HAC3JR   = "Age when 1st told you had goiter -- yrs"
        HAC3KR   = "Age 1st told had other thyr dis -- yrs"
        HAC3LR   = "Age when first told you had lupus -- yrs"
        HAC3MR   = "Age when first told you had gout -- yrs"
        HAC3NR   = "Age when 1st told had skin cancer -- yrs"
        HAC3OR   = "Age when 1st told had other cancr -- yrs"
        HAC3OS   = "Where was cancer located when first told"
        HAC4A    = "Doctor told relatives they had diabetes"
        HAC4B    = "Any relatives had heart attack before 50"
        HAC5A1   = "Did grandmother have diabetes"
        HAC5A2   = "Did grandfather have diabetes"
        HAC5A3   = "Did mother have diabetes"
        HAC5A4   = "Did father have diabetes"
        HAC5A5   = "Did sister have diabetes"
        HAC5A6   = "Did brother have diabetes"
        HAC5A7   = "Did aunt have diabetes"
        HAC5A8   = "Did uncle have diabetes"
        HAC5A9   = "Did cousin have diabetes"
        HAC5A10  = "Did son have diabetes"
        HAC5A11  = "Did daughter have diabetes"
        HAC5A12  = "Did other relative have diabetes"
        HAC5B1   = "Did grandmother have heart attack"
        HAC5B2   = "Did grandfather have heart attack"
        HAC5B3   = "Did mother have heart attack"
        HAC5B4   = "Did father have heart attack"
        HAC5B5   = "Did sister have heart attack"
        HAC5B6   = "Did brother have heart attack"
        HAC5B7   = "Did aunt have heart attack"
        HAC5B8   = "Did uncle have heart attack"
        HAC5B9   = "Did cousin have heart attack"
        HAC5B10  = "Did son have heart attack"
        HAC5B11  = "Did daughter have heart attack"
        HAC5B12  = "Did other relative have heart attack"
        HAC6     = "Doctor told mother she had osteoporosis"
        HAC7     = "Did your bio mother ever fracture hip"
        HAC7A    = "How many times mother fractured hip"
        HAC8     = "Age of mother at first hip fracture"
        HAD1     = "Ever been told you have sugar/diabetes"
        HAD2     = "Check item. Refer to sex of SP"
        HAD3     = "Were you pregnant when told had diabetes"
        HAD4     = "Other than pregnant,doctor told diabetes"
        HAD5R    = "Age first told you had diabetes -- yrs"
        HAD6     = "Are you now taking insulin"
        HAD7S    = "How often you take insulin - times/week"
        HAD8     = "On average, how many units per day taken"
        HAD9S    = "How long have you taken insulin - months"
        HAD10    = "Are you now taking diabetes pills"
        HAD11AS  = "How often check urine for sugar-times/mo"
        HAD11BS  = "How often check blood for sugar-times/mo"
        HAD12S   = "# days since last eye exam by doctor"
        HAD13S   = "# days since eye exam w/ pupils dilated"
        HAD14    = "Ever had photographs taken of retina"
        HAD15    = "Ever told diabetes affected your eyes"
        HAE1     = "How long since doctor took blood press"
        HAE2     = "Doctor ever told had hypertension/HBP"
        HAE3     = "Told 2+ times you had hypertension/HBP"
        HAE4A    = "Doctor told take prescribed med for HBP"
        HAE4B    = "Doctor told control/lose weight for HBP"
        HAE4C    = "Doctor told cut salt intake for HBP"
        HAE4D    = "Doctor told do anything else for HBP"
        HAE4D1   = "Doctor told exercise more for HBP"
        HAE4D2   = "Doctor told restrict alcohol for HBP"
        HAE4D3   = "Doctor told stop smoking for HBP"
        HAE4D4   = "Doctor told reduce tension for HBP"
        HAE4D5   = "Doctor told change diet for HBP"
        HAE4D6   = "Doctor told make other changes for HBP"
        HAE5A    = "Now taking prescribed medicine for HBP"
        HAE5B    = "Now controlling or losing weight for HBP"
        HAE5C    = "Now using less salt or sodium for HBP"
        HAE5D1   = "Now exercising for high blood pressure"
        HAE5D2   = "Now restricting alcohol for HBP"
        HAE5D3   = "Now quit smoking for high blood pressure"
        HAE5D4   = "Now reduced tension for HBP"
        HAE5D5   = "Now made diet changes for HBP"
        HAE5D6   = "Now made other changes for HBP"
        HAE6     = "Ever had blood cholesterol checked"
        HAE7     = "Doctor told blood cholesterol level high"
        HAE8A    = "Doctor told eat less fat for HBC"
        HAE8B    = "Doctor told lose weight for HBC"
        HAE8C    = "Doctor told exercise for HBC"
        HAE8D    = "Doctor told take medicine for HBC"
        HAE9A    = "Now eat less high fat foods for HBC"
        HAE9B    = "Now losing weight to lower cholesterol"
        HAE9C    = "Now exercising to lower cholesterol"
        HAE9D    = "Take prescribed med to lower cholesterol"
        HAE10    = "Check item. Mark first applicable box"
        HAE11A   = "On own for HBC, eat fewer high fat foods"
        HAE11B   = "On own for HBC, controlled/lost weight"
        HAE11C   = "On own for HBC, exercise more"
        HAF1     = "Ever had any pain or discomfort in chest"
        HAF2     = "Get chest pain when walk uphill or hurry"
        HAF3     = "Get chest pain if walk at ordinary pace"
        HAF4     = "What do if get chest pain while walking"
        HAF5     = "If stand still is chest pain relieved"
        HAF6     = "How soon is the chest pain relieved"
        HAF7A    = "Where is pain/discomfort located -area 1"
        HAF7B    = "Where is pain/discomfort located -area 2"
        HAF7C    = "Where is pain/discomfort located -area 3"
        HAF7D    = "Where is pain/discomfort located -area 4"
        HAF7E    = "Where is pain/discomfort located -area 5"
        HAF7F    = "Where is pain/discomfort located -area 6"
        HAF7G    = "Where is pain/discomfort located -area 7"
        HAF7H    = "Where is pain/discomfort located -area 8"
        HAF9     = "Ever had severe chest pain for half hr +"
        HAF10    = "Doctor ever told you had a heart attack"
        HAF11    = "How many heart attacks have you had"
        HAF12R   = "Age when you had 1st heart attack -- yrs"
        HAF13R   = "Age when had last heart attack -- yrs"
        HAF14    = "Get pain in either leg while walking"
        HAF15    = "Leg pain begin while stand still/sitting"
        HAF16    = "In what part of leg do you feel it"
        HAF17    = "Get leg pain when walk uphill or hurry"
        HAF18    = "Get leg pain,walk at ordinary pace,level"
        HAF19    = "Leg pain ever disappear while walking"
        HAF20    = "If leg pain: stop walking or continue"
        HAF21    = "Is leg pain relieved if you stand still"
        HAF22    = "How soon is leg pain relieved"
        HAF23    = "Weak/paralysis on face,arm,leg for >5min"
        HAF24    = "Numbness etc,1 side face/body for >5 min"
        HAF25    = "Loss of vision in 1/both eyes for >5 min"
        HAF26    = "Severe dizziness for more than 5 minutes"
        HAF27    = "Problem with ability to speak/understand"
        HAG1     = "Check item. Refer to age of SP"
        HAG2     = "Ever had back pain most days for 1+ mo"
        HAG3     = "Have you had back pain in past 12 months"
        HAG4A    = "What part of back is pain - area 1"
        HAG4B    = "What part of back is pain - area 2"
        HAG4C    = "What part of back is pain - area 3"
        HAG5A    = "Doctor told you had broken/fractured hip"
        HAG5B    = "Doctor told had broken/fractured wrist"
        HAG5C    = "Doctor told had broken/fractured spine"
        HAG6A    = "Number of times you fractured your hip"
        HAG6B    = "Number of times fractured your wrist"
        HAG6C    = "Number of times fractured your spine"
        HAG7     = "Check item. Refer to G5"
        HAG8A    = "Site of fracture 1"
        HAG8B    = "Site of fracture 2"
        HAG8C    = "Site of fracture 3"
        HAG8D    = "Site of fracture 4"
        HAG8E    = "Site of fracture 5"
        HAG8F    = "Site of fracture 6"
        HAG9AR   = "Age when fractured ...- 1st time -- yrs"
        HAG9BR   = "Age when fractured ...- 2nd time -- yrs"
        HAG9CR   = "Age when fractured ...- 3rd time -- yrs"
        HAG9DR   = "Age when fractured ...- 4th time -- yrs"
        HAG9ER   = "Age when fractured ...- 5th time -- yrs"
        HAG9FR   = "Age when fractured ...- 6th time -- yrs"
        HAG10A   = "Fracture 1: low/hi fall or severe trauma"
        HAG10B   = "Fracture 2: low/hi fall or severe trauma"
        HAG10C   = "Fracture 3: low/hi fall or severe trauma"
        HAG10D   = "Fracture 4: low/hi fall or severe trauma"
        HAG10E   = "Fracture 5: low/hi fall or severe trauma"
        HAG10F   = "Fracture 6: low/hi fall or severe trauma"
        HAG11    = "Doctor ever told you had osteoporosis"
        HAG12    = "Were you treated for osteoporosis"
        HAG13    = "Check item. Refer to age of SP"
        HAG14    = "# times fall on floor/object past 12 mo"
        HAG15    = "Fall cause broken bone, injury, med care"
        HAG16    = "Ever had hand pain most days for 6+ wks"
        HAG17A   = "Pain, stiff right hand, joint 1"
        HAG17B   = "Pain, stiff right hand, joint 2"
        HAG17C   = "Pain, stiff right hand, joint 3"
        HAG17D   = "Pain, stiff right hand, joint 4"
        HAG17E   = "Pain, stiff right hand, joint 5"
        HAG17F   = "Pain, stiff right hand, joint 6"
        HAG17G   = "Pain, stiff right hand, joint 7"
        HAG17H   = "Pain, stiff right hand, joint 8"
        HAG17I   = "Pain, stiff right hand, joint 9"
        HAG17J   = "Pain, stiff right hand, joint 10"
        HAG17K   = "Pain, stiff right hand, joint 11"
        HAG17L   = "Pain, stiff right hand, joint 12"
        HAG17M   = "Pain, stiff right hand, joint 13"
        HAG17N   = "Pain, stiff right hand, joint 14"
        HAG17O   = "Pain, stiff right hand, joint 15"
        HAG17FF  = "Pain, stiff right hand, joint 32"
        HAG17P   = "Pain, stiff left hand, joint 16"
        HAG17Q   = "Pain, stiff left hand, joint 17"
        HAG17R   = "Pain, stiff left hand, joint 18"
        HAG17S   = "Pain, stiff left hand, joint 19"
        HAG17T   = "Pain, stiff left hand, joint 20"
        HAG17U   = "Pain, stiff left hand, joint 21"
        HAG17V   = "Pain, stiff left hand, joint 22"
        HAG17W   = "Pain, stiff left hand, joint 23"
        HAG17X   = "Pain, stiff left hand, joint 24"
        HAG17Y   = "Pain, stiff left hand, joint 25"
        HAG17Z   = "Pain, stiff left hand, joint 26"
        HAG17AA  = "Pain, stiff left hand, joint 27"
        HAG17BB  = "Pain, stiff left hand, joint 28"
        HAG17CC  = "Pain, stiff left hand, joint 29"
        HAG17DD  = "Pain, stiff left hand, joint 30"
        HAG17EE  = "Pain, stiff left hand, joint 31"
        HAG18    = "Swelling in hands...most days for 6+ wks"
        HAG19A   = "Swelling in right hand, joint 1"
        HAG19B   = "Swelling in right hand, joint 2"
        HAG19C   = "Swelling in right hand, joint 3"
        HAG19D   = "Swelling in right hand, joint 4"
        HAG19E   = "Swelling in right hand, joint 5"
        HAG19F   = "Swelling in right hand, joint 6"
        HAG19G   = "Swelling in right hand, joint 7"
        HAG19H   = "Swelling in right hand, joint 8"
        HAG19I   = "Swelling in right hand, joint 9"
        HAG19J   = "Swelling in right hand, joint 10"
        HAG19K   = "Swelling in right hand, joint 11"
        HAG19L   = "Swelling in right hand, joint 12"
        HAG19M   = "Swelling in right hand, joint 13"
        HAG19N   = "Swelling in right hand, joint 14"
        HAG19O   = "Swelling in right hand, joint 15"
        HAG19FF  = "Swelling in right hand, joint 32"
        HAG19P   = "Swelling in left  hand, joint 16"
        HAG19Q   = "Swelling in left hand, joint 17"
        HAG19R   = "Swelling in left hand, joint 18"
        HAG19S   = "Swelling in left hand, joint 19"
        HAG19T   = "Swelling in left hand, joint 20"
        HAG19U   = "Swelling in left hand, joint 21"
        HAG19V   = "Swelling in left hand, joint 22"
        HAG19W   = "Swelling in left hand, joint 23"
        HAG19X   = "Swelling in left hand, joint 24"
        HAG19Y   = "Swelling in left hand, joint 25"
        HAG19Z   = "Swelling in left hand, joint 26"
        HAG19AA  = "Swelling in left hand, joint 27"
        HAG19BB  = "Swelling in left hand, joint 28"
        HAG19CC  = "Swelling in left hand, joint 29"
        HAG19DD  = "Swelling in left hand, joint 30"
        HAG19EE  = "Swelling in left hand, joint 31"
        HAG20    = "Stiff hands in morn most days for 6+ wks"
        HAG21    = "How long does morning stiffness last"
        HAG22    = "Ever have knee pain most days for 6+ wks"
        HAG23    = "Have pain in right, left, or both knees"
        HAG24    = "Does knee hurt at rest as well as moving"
        HAG25    = "Swelling in knees...most days for 6+ wks"
        HAG26    = "Swelling in right, left, or both knees"
        HAG27    = "Stiff knees in morn most days for 6+ wks"
        HAG28    = "Stiffness in right, left, or both knees"
        HAG29    = "How long morning knee stiffness last"
        HAG30    = "Ever had hip pain most days for 6+ weeks"
        HAG31    = "Have pain in right, left, or both hips"
        HAG32    = "Doctor ever told you had hip dysplasia"
        HAG33    = "Doctor ever told you had scoliosis"
        HAG34    = "Do you have chronic limp (lasted >3 mo)"
        HAH1     = "Difficulty walking a quarter of a mile"
        HAH2     = "Difficulty walking 10 steps without rest"
        HAH3     = "Difficulty stooping, crouching, kneeling"
        HAH4     = "Difficulty lifting or carrying 10 pounds"
        HAH5     = "Difficulty doing chores around the house"
        HAH6     = "Difficulty preparing own meals"
        HAH7     = "Difficulty managing your money"
        HAH8     = "Difficulty walking room to room, 1 level"
        HAH9     = "Difficulty standing from armless chair"
        HAH10    = "Difficulty getting in or out of bed"
        HAH11    = "Difficulty eating"
        HAH12    = "Difficulty dressing yourself"
        HAH13    = "Need help with personal care needs"
        HAH14    = "Need help in handling routine needs"
        HAH15    = "Use device to help get around: cane, etc"
        HAH16    = "Do you use any special eating utensils"
        HAH17    = "Use aids or devices to help you dress"
        HAJ0     = "Check item. Refer to age of SP"
        HAJ1     = "Pain in area in diagram in past 12 mo"
        HAJ2A    = "Most uncomfortable pain located - area 1"
        HAJ2B    = "Most uncomfortable pain located - area 2"
        HAJ2C    = "Most uncomfortable pain located - area 3"
        HAJ3S    = "Past 12 mo, longest episode of pain -min"
        HAJ4     = "How many days had pain in past 12 months"
        HAJ5     = "Was pain continuous or did come and go"
        HAJ6     = "If moved around,hurt more/less/no differ"
        HAJ7     = "Ever seen doctor about this pain"
        HAJ8     = "What did the doctor say caused the pain"
        HAJ9     = "Doctor ever told you had gallstones"
        HAJ10    = "Reason visit doctor when told gallstones"
        HAJ11    = "Gallstone medical treatment, not surgery"
        HAJ12    = "Have you ever had gallbladder surgery"
        HAJ13    = "Age when you had gallbladder surgery"
        HAJ14    = "Check item. Refer to J8, J10, and J12"
        HAJ15    = "Pain continue after gallbladder surgery"
        HAJ16    = "Doctor ever told you had gallstones"
        HAJ17    = "Have you ever had gallbladder surgery"
        HAJ18R   = "Age when had gallbladder surgery -- yrs"
        HAK1     = "Have you ever had kidney stones"
        HAK2     = "How many times you passed a kidney stone"
        HAK3A    = "Ever had medication for kidney stones"
        HAK3A1   = "Age had first kidney stone medication"
        HAK3B    = "Ever had lithotripsy for kidney stones"
		HAK3B1R  = "Age you had first lithotripsy-recode"
        HAK3C    = "Ever had surgery to remove kidney stones"
		HAK3C1R  = "Age 1st surgery for kidney stones-recode"
        HAK4     = "# times had bladder infection"
        HAK5     = "# of bladder infections in past 12 mos"
        HAK6     = "Symptoms of bladder infection for >3 mo"
        HAK7     = "Interstitial cystitis or painful bladder"
        HAK8     = "Age 1st had inter.cystitis/painful blad."
        HAK9     = "# times per night you get up to urinate"
        HAK10    = "When urinate,feel like bladder not empty"
        HAK11    = "Usually have trouble starting to urinate"
        HAK12    = "Check item. Refer to sex and age of SP"
        HAK13    = "Has urinary stream decreased over years"
        HAK14    = "Ever had non-cancer prostate surgery"
        HAL1     = "Cough most days, 3+ consecutive mo in yr"
        HAL2     = "How many years have you had this cough"
        HAL3     = "Bring up phlegm, 3+ consecutive mo in yr"
        HAL4     = "How many years had trouble with phlegm"
        HAL5     = "Shortness of breath, hurry/walk up hill"
        HAL6     = "Had wheezing,whistle in chest past 12 mo"
        HAL7     = "# episodes,wheezing/whistling past 12 mo"
        HAL8     = "# times in hospital, wheezing, past 12mo"
        HAL9     = "# times to doctor/ER, wheezing,past 12mo"
        HAL10    = "No cold but chest sounds wheezy/whistles"
        HAL11A   = "Stuffy, itchy, or runny nose, past 12 mo"
        HAL11B   = "Watery, itchy eyes in past 12 months"
        HAL12    = "# episodes itchy...nose/eyes, past 12 mo"
        HAL13    = "Check item. Refer to L6 - L11"
        HAL14A   = "Symptoms brought on by:exercise/cold air"
        HAL14B   = "Symptoms brought on by: animals"
        HAL14C   = "Symptoms brought on by: house dust"
        HAL14D   = "Symptoms brought on by: work environment"
        HAL14E   = "Symptoms brought on by: pollen"
        HAL15A   = "Month pollen symptoms worse: all months"
        HAL15B   = "Month pollen symptoms worse: January"
        HAL15C   = "Month pollen symptoms worse: February"
        HAL15D   = "Month pollen symptoms worse: March"
        HAL15E   = "Month pollen symptoms worse: April"
        HAL15F   = "Month pollen symptoms worse: May"
        HAL15G   = "Month pollen symptoms worse: June"
        HAL15H   = "Month pollen symptoms worse: July"
        HAL15I   = "Month pollen symptoms worse: August"
        HAL15J   = "Month pollen symptoms worse: September"
        HAL15K   = "Month pollen symptoms worse: October"
        HAL15L   = "Month pollen symptoms worse: November"
        HAL15M   = "Month pollen symptoms worse: December"
        HAL16A   = "Severe reaction from insect sting"
        HAL16B   = "Severe reaction < one hour after eating"
        HAL17    = "Severe reaction after allergy shot/test"
        HAL18    = "Ever give up/avoid pet due to allergies"
        HAL19A   = "In past 12 months had: cold or flu"
        HAL19B   = "In past 12 mo had: sinusitis/sinus prob"
        HAL19C   = "In past 12 months had: pneumonia"
        HAL20A   = "# episodes in past 12 month: cold or flu"
        HAL20B   = "# episodes in past 12 months: sinusitis"
        HAL20C   = "# episodes in past 12 months: pneumonia"
        HAM1     = "Check item. Refer to age of SP"
        HAM2     = "Receive meals from senior program"
        HAM3     = "Are senior meals delivered to your home"
        HAM4     = "How often do you eat breakfast"
        HAM5S    = "How tall are you without shoes - inches"
        HAM6S    = "How much do you weigh w/out clothes -lbs"
        HAM7     = "Check item. Refer to age of SP"
        HAM8S    = "How much did you weigh 10 years ago -lbs"
        HAM9S    = "How much did you weigh at age 25 - lbs"
        HAM10S   = "Up to present time,most ever weighed-lbs"
        HAM11    = "Consider self over/under/right weight"
        HAM12    = "Would you like to weigh more,less,same"
        HAM13    = "Tried to lose weight in past 12 months"
        HAM14    = "Past 12 mo, changed what eat, med reason"
        HAM15A   = "Medical reason to change eating: obesity"
        HAM15B   = "Medical reason to change eating: high BP"
        HAM15C   = "Med reason to change eating: cholesterol"
        HAM15D   = "Medical reason to change eating:diabetes"
        HAM15E   = "Med reason change eating: heart disease"
        HAM15F   = "Medical reason to change eating: allergy"
        HAM15G   = "Medical reason to change eating: ulcer"
        HAM15H   = "Med reason to change eating:other reason"
        HAM15K   = "Med reason to change eating: pregnancy"
        HAM15L   = "Med reason to change eating: arthritis"
        HAM15M   = "Med reason to change eating: back pain"
        HAM15N   = "Med reason change eating:stomach/gastric"
        HAM15O   = "Med reason change eating: cancer/tumors"
        HAM15P   = "Med reason change eat: dental/oral probs"
        HAM15Q   = "Med reason to change eating:constipation"
        HAM15R   = "Med reason change eat: gallstone/bladder"
        HAM15S   = "Med reason change eat: intestinal probs"
        HAM15T   = "Medical reason to change eating: hernia"
        HAM15U   = "Medical reason to change eating: anemia"
        HAM15V   = "Medical reason to change eating: stroke"
        HAM15W   = "Med reason change eating: psychological"
        HAM15X   = "Med to change eating:infections/viruses"
        HAM15Y   = "Medical reason to change eating: dietary"
        HAM15Z   = "Med reason to change eat: general health"
        HAN1AS   = "Chocolate milk and hot cocoa - times/mo"
        HAN1BS   = "Milk to drink or on cereal - times/month"
        HAN1C    = "Question 1a check item, used in skip"
        HAN1D    = "What type of milk - whole, 2%, 1%, skim"
        HAN1ES   = "Yogurt and frozen yogurt - times/month"
        HAN1FS   = "Ice cream,ice milk,milkshakes - times/mo"
        HAN1GS   = "Cheese, all types - times/month"
        HAN1HS   = "Pizza, calzone, lasagna - times/month"
        HAN1IS   = "Cheese dishes - times/month"
        HAN2AS   = "Stew or soup with vegetables - times/mo"
        HAN2BS   = "Spaghetti/pasta w/ tomato sauce-times/mo"
        HAN2CS   = "Bacon/sausage/processed meats - times/mo"
        HAN2DS   = "Liver and other organ meats - times/mo"
        HAN2ES   = "Beef - times/month"
        HAN2FS   = "Pork and ham - times/month"
        HAN2GS   = "Shrimp, clams, etc - times/mo"
        HAN2HS   = "Fish - times/month"
        HAN2IS   = "Chicken and turkey - times/month"
        HAN2JS   = "Eggs - times/month"
        HAN3AS   = "Orange juice, etc - times/month"
        HAN3BS   = "Other fruit juices - times/month"
        HAN3CS   = "Citrus fruits - times/month"
        HAN3DS   = "Melons - times/month"
        HAN3ES   = "Peaches, nectarines, etc - times/month"
        HAN3FS   = "Any other fruits - times/month"
        HAN4AS   = "Carrots - times/month"
        HAN4BS   = "Broccoli - times/month"
        HAN4CS   = "Brussel sprouts/cauliflower - times/mo"
        HAN4DS   = "White potatoes - times/month"
        HAN4ES   = "Sweet potatoes, yams, etc - times/month"
        HAN4FS   = "Tomatoes - times/month"
        HAN4GS   = "Spinach, greens, etc - times/month"
        HAN4HS   = "Tossed salad - times/month"
        HAN4IS   = "Cabbage, cole slaw, sauerkraut-times/mo"
        HAN4JS   = "Hot red chili peppers - times/month"
        HAN4KS   = "Other peppers - times/month"
        HAN4LS   = "Any other vegetables - times/month"
        HAN5AS   = "Beans, lentils, chickpeas - times/month"
        HAN5BS   = "Peanuts, peanut butter, etc - times/mo"
        HAN5CS   = "Cereals: All-Bran, etc - times/month"
        HAN5DS   = "Cereals: Total, etc - times/month"
        HAN5ES   = "All other cold cereals - times/month"
        HAN5FS   = "Cooked, hot cereals - times/month"
        HAN5GS   = "White bread, rolls, etc - times/month"
        HAN5HS   = "Dark breads and rolls - times/month"
        HAN5IS   = "Corn bread, muffins, tortillas-times/mo"
        HAN5JS   = "Flour tortillas - times/month"
        HAN5KS   = "Rice - times/month"
        HAN5LS   = "Salted snacks - times/month"
        HAN6AS   = "Cakes, cookies, brownies, etc - times/mo"
        HAN6BS   = "Chocolate candy and fudge - times/month"
        HAN6CS   = "Hi-C, Tang, Koolaid, etc - times/month"
        HAN6DS   = "Diet colas, diet sodas, etc - times/mo"
        HAN6ES   = "Regular colas and sodas - times/month"
        HAN6FS   = "Regular coffee - times/month"
        HAN6GS   = "Regular tea - times/month"
        HAN6HS   = "Beer and lite beer - times/month"
        HAN6IS   = "Wine, etc - times/month"
        HAN6JS   = "Hard liquor - times/month"
        HAN7AS   = "Margarine - times/month"
        HAN7BS   = "Butter - times/month"
        HAN7CS   = "Oil/vinegar, mayonnaise, etc - times/mo"
        HAN8     = "Have I missed any foods/bev?"
        HAN8ACD  = "1st other food or beverage - specified"
        HAN8AS   = "1st other food or beverage - times/month"
        HAN8BCD  = "2nd other food or beverage - specified"
        HAN8BS   = "2nd other food or beverage - times/month"
        HAN8CCD  = "3rd other food or beverage - specified"
        HAN8CS   = "3rd other food or beverage - times/month"
        HAN8DCD  = "4th other food or beverage - specified"
        HAN8DS   = "4th other food or beverage - times/month"
        HAN8ECD  = "5th other food or beverage - specified"
        HAN8ES   = "5th other food or beverage - times/month"
        HAN8FCD  = "6th other food or beverage - specified"
        HAN8FS   = "6th other food or beverage - times/month"
        HAN9     = "Check item. Refer to age of SP"
        HAN10A   = "How often drink milk when child (5-12)"
        HAN10B   = "How often drnk milk when teenager(13-17)"
        HAN10C   = "How often drink milk, young adult(18-35)"
        HAN10D   = "How often drnk milk,mid-age adult(36-65)"
        HAN10E   = "How often drink milk,older adult(over65)"
        HAP1     = "Have total blindness in one or both eyes"
        HAP1A    = "Total blindness in 1 eye or in both eyes"
        HAP2     = "Do you use glasses, contacts, or both"
        HAP3     = "Trouble seeing w/one/both eyes w/glasses"
        HAP4     = "W/glasses recognize friend across street"
        HAP5     = "W/ glasses recognize friend across room"
        HAP6     = "W/ glasses recognize friend arms length"
        HAP7     = "W/glasses recognize friend face,close up"
        HAP8     = "W/ glasses see enough to read news print"
        HAP9     = "W/ glasses see enough to read headlines"
        HAP10    = "Have total deafness in one or both ears"
        HAP10A   = "Total deafness in 1 ear or in both ears"
        HAP11    = "Do you use a hearing aid"
        HAP12    = "Have trouble hearing even w/hearing aid"
        HAP13    = "W/ aid can hear most things people say"
        HAP14    = "W/aid can hear only few words/loud noise"
        HAP15    = "Difficulty using phone, hearing problems"
        HAP16    = "Check item. Refer to age of SP"
        HAP17A1  = "Repeat item: 'apple' (first trial)"
        HAP17A2  = "Repeat item: 'table' (first trial)"
        HAP17A3  = "Repeat item: 'penny' (first trial)"
        HAP17B   = "Number of trials until objects learned"
        HAP18A   = "Dollars left: have $20 and take away $3"
        HAP18B   = "Dollars left: subtract $3 from $17"
        HAP18C   = "Dollars left: subtract $3 from $14"
        HAP18D   = "Dollars left: subtract $3 from $11"
        HAP18E   = "Dollars left: subtract $3 from $8"
        HAP19A   = "Repeat item: 'apple' (second trial)"
        HAP19B   = "Repeat item: 'table' (second trial)"
        HAP19C   = "Repeat item: 'penny' (second trial)"
        HAQ1     = "Describe natural teeth: excellent...poor"
        HAQ2A    = "Dental care needed:teeth filled/replaced"
        HAQ2B    = "Dental care needed: teeth pulled"
        HAQ2C    = "Dental care needed: gum treatment"
        HAQ2D    = "Dental care needed: denture work"
        HAQ2E    = "Dental care needed: relief of pain"
        HAQ2F    = "Dental care needed:to improve appearance"
        HAQ2G    = "Dental care needed: other"
        HAQ2I    = "Dental care needed: nothing"
        HAQ2K    = "Dental care needed: cleaning"
        HAQ2L    = "Dental care needed: checkup"
        HAQ3S    = "# days ago last visit dentist/hygienist"
        HAQ4     = "How often do you go to dentist/hygienist"
        HAQ5     = "Past 12 mo,had cold sores/fever blisters"
        HAQ6     = "Past 12 months, had canker/mouth sores"
        HAQ7     = "Check item. Refer to age of SP"
        HAQ8     = "Ever received orthodontic treatment"
        HAQ9     = "Age started recent orthodontic treatment"
        HAR1     = "Have you smoked 100+ cigarettes in life"
        HAR2     = "Age when you started smoking regularly"
        HAR3     = "Do you smoke cigarettes now"
        HAR4S    = "# cigarettes smoked per day"
        HAR5     = "How many yrs have you smoked this amount"
        HAR6     = "Ever period of 1+ years when smoked more"
        HAR7S    = "# cigarettes per day when smoking most"
        HAR8     = "How many years did you smoke that amount"
        HAR9     = "Ever quit smoking for one year or longer"
        HAR10    = "How many years not smoked since started"
        HAR11R   = "Age last smoke cigarettes reg -- yrs"
        HAR12S   = "# cigarettes smoked per day at that time"
        HAR13    = "Quit smoking,health problem from smoking"
        HAR14    = "Ever used chewing tabacco or snuff"
        HAR15    = "Age 1st use chew tobacco/snuff regularly"
        HAR16    = "Do you use chewing tobacco or snuff now"
        HAR17    = "Which - chewing tobacco or snuff"
        HAR18AS  = "# containers of snuff used per week"
        HAR18BS  = "# containers of chewing tobacco per week"
        HAR19A   = "Place in right, left, or front of mouth"
        HAR19B   = "Place in top or bottom of mouth"
        HAR20R   = "Age last use chew tob/snuf reg -- yrs"
        HAR21    = "Which used, chewing tobacco or snuff"
        HAR22    = "Quit chew/snuff, related health problem"
        HAR23    = "Ever smoke at least 20 cigars in life"
        HAR24    = "Do you now smoke cigars"
        HAR25    = "How many cigars do you smoke per day"
        HAR26    = "Ever smoke 20 pipes of tobacco in life"
        HAR27    = "Do you now smoke a pipe"
        HAR28    = "How many pipefuls tobacco smoke per day"
        HAS1     = "Past 2 wks, did you work at job/business"
        HAS2     = "Past 2 weeks, did you have job/business"
        HAS3     = "Looking for work or on layoff from job"
        HAS4     = "Which, looking for work or on layoff"
        HAS5     = "Check item.  Refer to S2"
        HAS8R    = "What kind of business or industry -- rcd"
        HAS9R    = "What kind of work were you doing -- rcd"
        HAS11    = "Class of worker"
        HAS12S   = "How long in months worked at that job"
        HAS13    = "Check item. Refer to S1 and S2"
        HAS14    = "At work, hours per day can smell smoking"
        HAS15    = "In job, ever wear protective equipment"
        HAS16A   = "Ever wear eye protection"
        HAS16B   = "Ever wear protective gloves"
        HAS16C   = "Ever wear dust mask"
        HAS16D   = "Ever wear respirator"
        HAS17    = "What kind of work were you doing longest"
        HAS17R   = "What kind of work doing longest  -- rcd"
        HAS18    = "How long this kind of work-all employers"
        HAS19    = "What business/industry worked in longest"
        HAS19R   = "Business/industry worked in longest--rcd"
        HAS21    = "Check item. Refer to S1 and S12"
        HAS22    = "What were you doing most of past 12 mo"
        HAS23    = "Limited in work by health problem"
        HAS24    = "Limited in housework by health problem"
        HAS25    = "Limited in activities by health problem"
        HAS26    = "Ever change work/housework, health prob"
        HAS27A   = "Did you change permanently to easier job"
        HAS27B   = "Did you change temporarily to easier job"
        HAS27C   = "Did you cut down to part-time work only"
        HAS27D   = "Did you have to stop work for few months"
        HAS27E   = "Did you retire because of a disability"
        HAS27F   = "Did you have to cut down on housework"
        HAS27G   = "Did you stop doing all housework"
        HAS27H   = "Did you make some other change"
        HAS27HCD = "Other specify - coded"
        HAS28    = "Language you mainly speak at home"
        HAS29    = "Language you mainly speak at work/school"
        HAT1S    = "Past mo,how often walk mile without stop"
        HAT1MET  = "Walking activity - intensity rating"
        HAT2     = "In the past month, did you jog or run"
        HAT2MET  = "Jog or run activity - intensity rating"
        HAT3S    = "Past month, how often did you jog or run"
        HAT4     = "Past mo, ride bicycle/exercise bicycle"
        HAT4MET  = "Bicycle activity - intensity rating"
        HAT5S    = "Past mo, how often ride bike/exer bike"
        HAT6     = "In the past month did you swim"
        HAT6MET  = "Swim activity - intensity rating"
        HAT7S    = "Past month, how often did you swim"
        HAT8     = "Past mo, do aerobics or aerobic dancing"
        HAT8MET  = "Aerobics activity - intensity rating"
        HAT9S    = "Past mo, how often did aerobics/dancing"
        HAT10    = "Past month, did you do other dancing"
        HAT10MET = "Other dancing activity -intensity rating"
        HAT11S   = "Past month, how often did other dancing"
        HAT12    = "Past month, do calisthenics or exercises"
        HAT12MET = "Calisthenics activity - intensity rating"
        HAT13S   = "Past mo, how often did calisthenics/exer"
        HAT14    = "Past month, did you do garden/yard work"
        HAT14MET = "Garden/yard activity - intensity rating"
        HAT15S   = "Past mo, how often did garden/yard work"
        HAT16    = "In the past month, did you lift weights"
        HAT16MET = "Lift weights activity - intensity rating"
        HAT17S   = "Past mo, how often did you lift weights"
        HAT18    = "Past mo, any other exercises, sports ..."
        HAT19CD  = "PROBE: What - coded"
        HAT19MET = "Other activity - intensity rating"
        HAT20S   = "Past mo, how often did other activity"
        HAT21CD  = "PROBE: Any others - coded (activity 2)"
        HAT21MET = "Other activity 2 - intensity rating"
        HAT22S   = "Past mo, how often did other activity 2"
        HAT23CD  = "PROBE: Any others - coded (activity 3)"
        HAT23MET = "Other activity 3 - intensity rating"
        HAT24S   = "Past mo, how often did other activity 3"
        HAT25CD  = "PROBE: Any others - Coded (activity 4)"
        HAT25MET = "Other activity 4 - intensity rating"
        HAT26S   = "Past mo, how often did other activity 4"
        HAT27    = "Compare activity for past mo to past yr"
        HAT28    = "Active compared with men/women your age"
        HAT29    = "Check item. Refer to age of SP"
        HAT30    = "Active now compared with self 10 yrs ago"
        HAV1S    = "# times/week on phone w/ family, friends"
        HAV2S    = "How often per yr together w/friends/rels"
        HAV3S    = "How often per year you visit w/neighbors"
        HAV4S    = "How often per yr attend church/services"
        HAV5     = "Do you belong to any clubs or groups"
        HAV6S    = "How often per year attend club meetings"
        HAV7R    = "Weeks lived at this address -- recode"
        HAV8     = "How long lived in this city/town/area"
        HAV9     = "Have you lived anywhere longer than here"
        HAV10R   = "Where did you live longest -- recode"
        HAV11    = "How long did you live in longest place"
        HAV12R   = "Where were you born -- recode"
        HAV13R   = "If not born US, how long lived US -- yrs"
        HAX1A    = "Taken vitamins/minerals in past month"
        HAX1B    = "Taken prescriptn medicines in past month"
        HAX1C    = "Taken antacids in past month"
        HAX2A    = "How many vitamins/minerals taken"
        HAX2B    = "How many prescription medicines taken"
        HAX2C    = "How many antacids taken"
        HAX3CG   = "Certainty index code-1st"
        HAX3DG   = "Product class-1st"
        HAX6SG   = "How often did you take in past month-1st"
        HAX8SG   = "How long been taking (month)-1st"
        HAX3CH   = "Certainty index code-2nd"
        HAX3DH   = "Product class-2nd"
        HAX6SH   = "How often did you take in past month-2nd"
        HAX8SH   = "How long been taking (month)-2nd"
        HAX3CI   = "Certainty index code-3rd"
        HAX3DI   = "Product class-3rd"
        HAX6SI   = "How often did you take in past month-3rd"
        HAX8SI   = "How long been taking (month)-3rd"
        HAX3CJ   = "Certainty index code-4th"
        HAX3DJ   = "Product class-4th"
        HAX6SJ   = "How often did you take in past month-4th"
        HAX8SJ   = "How long been taking (month)-4th"
        HAX3CK   = "Certainty index code-5th"
        HAX3DK   = "Product class-5th"
        HAX6SK   = "How often did you take in past month-5th"
        HAX8SK   = "How long been taking (month)-5th"
        HAX3CL   = "Certainty index code-6th"
        HAX3DL   = "Product class-6th"
        HAX6SL   = "How often did you take in past month-6th"
        HAX8SL   = "How long been taking (month)-6th"
        HAX3CM   = "Certainty index code-7th"
        HAX3DM   = "Product class-7th"
        HAX6SM   = "How often did you take in past month-7th"
        HAX8SM   = "How long been taking (month)-7th"
        HAX3CN   = "Certainty index code-8th"
        HAX3DN   = "Product class-8th"
        HAX6SN   = "How often did you take in past month-8th"
        HAX8SN   = "How long been taking (month)-8th"
        HAX3CO   = "Certainty index code-9th"
        HAX3DO   = "Product class-9th"
        HAX6SO   = "How often did you take in past month-9th"
        HAX8SO   = "How long been taking (month)-9th"
        HAX3CP   = "Certainty index code-10th"
        HAX3DP   = "Product class-10th"
        HAX6SP   = "How often did you take in past mnth-10th"
        HAX8SP   = "How long been taking (month)-10th"
        HAX3CQ   = "Certainty index code-11th"
        HAX3DQ   = "Product class-11th"
        HAX6SQ   = "How often did you take in past mnth-11th"
        HAX8SQ   = "How long been taking (month)-11th"
        HAX3CR   = "Certainty index code-12th"
        HAX3DR   = "Product class-12th"
        HAX6SR   = "How often did you take in past mnth-12th"
        HAX8SR   = "How long been taking (month)-12th"
        HAX3CS   = "Certainty index code-13th"
        HAX3DS   = "Product class-13th"
        HAX6SS   = "How often did you take in past mnth-13th"
        HAX8SS   = "How long been taking (month)-13th"
        HAX3CT   = "Certainty index code-14th"
        HAX3DT   = "Product class-14th"
        HAX6ST   = "How often did you take in past mnth-14th"
        HAX8ST   = "How long been taking (month)-14th"
        HAX3CU   = "Certainty index code-15th"
        HAX3DU   = "Product class-15th"
        HAX6SU   = "How often did you take in past mnth-15th"
        HAX8SU   = "How long been taking (month)-15th"
        HAX3CV   = "Certainty index code-16th"
        HAX3DV   = "Product class-16th"
        HAX6SV   = "How often did you take in past mnth-16th"
        HAX8SV   = "How long been taking (month)-16th"
        HAX3CW   = "Certainty index code-17th"
        HAX3DW   = "Product class-17th"
        HAX6SW   = "How often did you take in past mnth-17th"
        HAX8SW   = "How long been taking (month)-17th"
        HAX9DG   = "Primary drug class code-1st"
        HAX9EG   = "Secondary drug class code-1st"
        HAX9FG   = "Tertiary drug class code-1st"
        HAX11AG  = "ICD-9-CM code-1st"
        HAX12SG  = "For how long been taking (days)-1st"
        HAX9DH   = "Primary drug class code-2nd"
        HAX9EH   = "Secondary drug class code-2nd"
        HAX9FH   = "Tertiary drug class code-2nd"
        HAX11AH  = "ICD-9-CM code-2nd"
        HAX12SH  = "For how long been taking (days)-2nd"
        HAX9DI   = "Primary drug class code-3rd"
        HAX9EI   = "Secondary drug class code-3rd"
        HAX9FI   = "Tertiary drug class code-3rd"
        HAX11AI  = "ICD-9-CM code-3rd"
        HAX12SI  = "For how long been taking (days)-3rd"
        HAX9DJ   = "Primary drug class code-4th"
        HAX9EJ   = "Secondary drug class code-4th"
        HAX9FJ   = "Tertiary drug class code-4th"
        HAX11AJ  = "ICD-9-CM code-4th"
        HAX12SJ  = "For how long been taking (days)-4th"
        HAX9DK   = "Primary drug class code-5th"
        HAX9EK   = "Secondary drug class code-5th"
        HAX9FK   = "Tertiary drug class code-5th"
        HAX11AK  = "ICD-9-CM code-5th"
        HAX12SK  = "For how long been taking (days)-5th"
        HAX9DL   = "Primary drug class code-6th"
        HAX9EL   = "Secondary drug class code-6th"
        HAX9FL   = "Tertiary drug class code-6th"
        HAX11AL  = "ICD-9-CM code-6th"
        HAX12SL  = "For how long been taking (days)-6th"
        HAX9DM   = "Primary drug class code-7th"
        HAX9EM   = "Secondary drug class code-7th"
        HAX9FM   = "Tertiary drug class code-7th"
        HAX11AM  = "ICD-9-CM code-7th"
        HAX12SM  = "For how long been taking (days)-7th"
        HAX9DN   = "Primary drug class code-8th"
        HAX9EN   = "Secondary drug class code-8th"
        HAX9FN   = "Tertiary drug class code-8th"
        HAX11AN  = "ICD-9-CM code-8th"
        HAX12SN  = "For how long been taking (days)-8th"
        HAX9DO   = "Primary drug class code-9th"
        HAX9EO   = "Secondary drug class code-9th"
        HAX9FO   = "Tertiary drug class code-9th"
        HAX11AO  = "ICD-9-CM code-9th"
        HAX12SO  = "For how long been taking (days)-9th"
        HAX9DP   = "Primary drug class code-10th"
        HAX9EP   = "Secondary drug class code-10th"
        HAX9FP   = "Tertiary drug class code-10th"
        HAX11AP  = "ICD-9-CM code-10th"
        HAX12SP  = "For how long been taking (days)-10th"
        HAX9DQ   = "Primary drug class code-11th"
        HAX9EQ   = "Secondary drug class code-11th"
        HAX9FQ   = "Tertiary drug class code-11th"
        HAX11AQ  = "ICD-9-CM code-11th"
        HAX12SQ  = "For how long been taking (days)-11th"
        HAX9DR   = "Primary drug class code-12th"
        HAX9ER   = "Secondary drug class code-12th"
        HAX9FR   = "Tertiary drug class code-12th"
        HAX11AR  = "ICD-9-CM code-12th"
        HAX12SR  = "For how long been taking (days)-12th"
        HAX9DS   = "Primary drug class code-13th"
        HAX9ES   = "Secondary drug class code-13th"
        HAX9FS   = "Tertiary drug class code-13th"
        HAX11AS  = "ICD-9-CM code-13th"
        HAX12SS  = "For how long been taking (days)-13th"
        HAX9DT   = "Primary drug class code-14th"
        HAX9ET   = "Secondary drug class code-14th"
        HAX9FT   = "Tertiary drug class code-14th"
        HAX11AT  = "ICD-9-CM code-14th"
        HAX12ST  = "For how long been taking (days)-14th"
        HAX9DU   = "Primary drug class code-15th"
        HAX9EU   = "Secondary drug class code-15th"
        HAX9FU   = "Tertiary drug class code-15th"
        HAX11AU  = "ICD-9-CM code-15th"
        HAX12SU  = "For how long been taking (days)-15th"
        HAX9DV   = "Primary drug class code-16th"
        HAX9EV   = "Secondary drug class code-16th"
        HAX9FV   = "Tertiary drug class code-16th"
        HAX11AV  = "ICD-9-CM code-16th"
        HAX12SV  = "For how long been taking (days)-16th"
        HAX13AG  = "Antacid identification code-1st"
        HAX15SG  = "How often did you take in past month-1st"
        HAX16AG  = "How much did you take ea time (quan)-1st"
		HAX16A1G = "How much did you take ea time (unit)-1st" 
        HAX17SG  = "How long been taking (months)-1st"
        HAX13AH  = "Antacid identification code-2nd"
        HAX15SH  = "How often did you take in past month-2nd"
        HAX16AH  = "How much did you take ea time (quan)-2nd"
		HAX16A1H = "How much did you take ea time (unit)-2nd"
        HAX17SH  = "How long been taking (months)-2nd"
        HAX13AI  = "Antacid identification code-3rd"
        HAX15SI  = "How often did you take in past month-3rd"
        HAX16AI  = "How much did you take ea time (quan)-3rd"
		HAX16A1I = "How much did you take ea time (unit)-3rd"
        HAX17SI  = "How long been taking (months)-3rd"
        HAX13AJ  = "Antacid identification code-4th"
        HAX15SJ  = "How often did you take in past month-4th"
        HAX16AJ  = "How much did you take ea time (quan)-4th"
		HAX16A1J = "How much did you take ea time (unit)-4th"
        HAX17SJ  = "How long been taking (months)-4th"
        HAX18A   = "Taken aspirin, etc. in past month"
        HAX19AS  = "How often did you take in past month"
        HAX18B   = "Taken acetaminophen, etc. in past month"
        HAX19BS  = "How often did you take in past month"
        HAX18C   = "Taken ibuprofen, etc. in past month"
        HAX19CS  = "How often did you take in past month"
        HAX18D   = "Taken other pain rel. in past month-1st"
        HAX18DB  = "Other pain rel. product type-1st"
        HAX19DS  = "How often did you take in past month-1st"
        HAX18E   = "Taken other pain rel. in past month-2nd"
        HAX18EB  = "Other pain rel. product type-2nd"
        HAX19ES  = "How often did you take in past month-2nd"
        HAX18F   = "Taken other pain rel. in past month-3rd"
        HAX18FB  = "Other pain rel. product type-3rd"
        HAX19FS  = "How often did you take in past month-3rd"
        HAX18G   = "Taken other pain rel. in past month-4th"
        HAX18GB  = "Other pain rel. product type-4th"
        HAX19GS  = "How often did you take in past month-4th"
        HAY6     = "Main respondent for household adult quex"
        HAY7     = "Reason for accepting proxy respondent"
        HAY8     = "Was SP present during any of interview"
        HAY9     = "Anyone else present during interview"
        HAY10    = "Quality of interview"
        HAY11A   = "Observed SP: in bed"
        HAY11B   = "Observed SP: in wheelchair"
        HAY11C   = "Observed SP:uses cane,crutches,or walker"
        HAY11D   = "Observed SP: walks slowly or shuffles"
        HAY11E   = "Observed SP: paralysis in hands or legs"
        HAY11F   = "Observed SP: hearing impairment"
        HAY11G   = "Observe SP:speech problems(not language)"
        HAY11H   = "Observed SP: coughs continuously"
        HAY11I   = "Observed SP: dressed in street clothes"
        HAY11J   = "Observed SP:overweight, average, or thin"
        HAZA1    = "Any medical reason procedure not be done"
        HAZA1A   = "Med reason BP not taken(survey years4-6)"
        HAZA1CC  = "Med reason BP not taken - other(yrs 5-6)"
        HAZA2    = "Have you consumed food, etc last 30 min"
        HAZA2A1  = "Have you consumed food in last 30 min"
        HAZA2A2  = "Have you consumed alcohol in last 30 min"
        HAZA2A3  = "Have you consumed coffee in last 30 min"
        HAZA2A4  = "Have you smoked cigarettes last 30 min"
        HAZA3    = "Cuff size used"
        HAZA4    = "Arm selected"
        HAZA4A   = "Reason left arm selected(survey yrs 4-6)"
        HAZA5    = "Radial pulse (30 seconds)"
        HAZA5R   = "Radial pulse rate (beats/min)"
        HAZA6    = "Radial pulse regular/rhythmic"
        HAZA7A   = "First maximum inflation rate (MIL, mmHg)"
        HAZA7AA  = "Reason first MIL not taken (years 4-6)"
        HAZA7B   = "Second MIL (mm Hg) (survey years 4-6)"
        HAZA7BA  = "Reason second MIL not taken (years 4-6)"
        HAZA8AK1 = "K1 for first BP measure (systolic, mmHg)"
        HAZA8AK5 = "K5 for first BP measure (diastolic,mmHg)"
        HAZA8A3  = "Reason first BP measurement not done"
        HAZA8A4  = "Reason first BP measurement refused"
        HAZA8BK1 = "K1 for second BP measure (systolic,mmHg)"
        HAZA8BK5 = "K5 for second BP measure(diastolic,mmHg)"
        HAZA8B3  = "Reason second BP measurement not done"
        HAZA8B4  = "Reason second BP measurement refused"
        HAZA8CK1 = "K1 for third BP measure (systolic, mmHg)"
        HAZA8CK5 = "K5 for third BP measure (diastolic,mmHg)"
        HAZA8C3  = "Reason third BP measurement not done"
        HAZA8C4  = "Reason third BP measurement refused"
        HAZA8DK1 = "K1 for fourth BP (systolic,mmHg,yrs 4-6)"
        HAZA8DK5 = "K5 for fourth BP(diastolic,mmHg,yrs 4-6)"
        HAZA8D3  = "Reason fourth BP not done (years 4-6)"
        HAZA8D4  = "Reason fourth BP refused (years 4-6)"
        HAZA9    = "Reason for not taking BP (years 5-6)"
        HAZA10   = "Code determining BP averaging (yrs 5-6)"
        HAZA11A  = "Average K1 BP (survey years 5 and 6)"
        HAZA11AR = "Average K1 BP after data edits"
        HAZA11B  = "Average K5 BP (survey years 5-6)"
        HAZA11BR = "Average K5 BP after data edits"
        HAZA12   = "BP statement read to SP (years 5 and 6)"
        HAZMNK1R = "Average K1 BP from household and MEC"
        HAZNOK1R = "Number of BP's used for average K1"
        HAZMNK5R = "Average K5 BP from household and MEC"
        HAZNOK5R = "Number of BP's used for average K5";

