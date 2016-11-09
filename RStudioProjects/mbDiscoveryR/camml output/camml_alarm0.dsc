// ~->[DNET-1]->~

// File created by someone at MonashU using Netica 5.15 on Nov 09, 2016 at 17:33:54.

bnet camml_alarm0 {
autoupdate = TRUE;
whenchanged = 1478673234;

visual V1 {
	defdispform = BELIEFBARS;
	nodelabeling = TITLE;
	NodeMaxNumEntries = 50;
	nodefont = font {shape= "Arial"; size= 9;};
	linkfont = font {shape= "Arial"; size= 9;};
	windowposn = (25, 25, 1220, 502);
	resolution = 72;
	drawingbounds = (2610, 3869);
	showpagebreaks = FALSE;
	usegrid = TRUE;
	gridspace = (6, 6);
	NodeSet Node {BuiltIn = 1; Color = 0x00E1E1E1;};
	NodeSet Nature {BuiltIn = 1; Color = 0x00F8EED2;};
	NodeSet Deterministic {BuiltIn = 1; Color = 0x00D3CAA6;};
	NodeSet Finding {BuiltIn = 1; Color = 0x00C8C8C8;};
	NodeSet Constant {BuiltIn = 1; Color = 0x00FFFFFF;};
	NodeSet ConstantValue {BuiltIn = 1; Color = 0x00FFFFB4;};
	NodeSet Utility {BuiltIn = 1; Color = 0x00FFBDBD;};
	NodeSet Decision {BuiltIn = 1; Color = 0x00DEE8FF;};
	NodeSet Documentation {BuiltIn = 1; Color = 0x00F0FAFA;};
	NodeSet Title {BuiltIn = 1; Color = 0x00FFFFFF;};
	PrinterSetting A {
		margins = (1270, 1270, 1270, 1270);
		};
	};

node HYP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.7977351,   0.2022649);
	whenchanged = 0;
	visual V1 {
		center = (174, 72);
		height = 17;
		};
	};

node LVF {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.9504775,   0.04952252);
	whenchanged = 0;
	visual V1 {
		center = (522, 72);
		height = 18;
		};
	};

node LVV {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (HYP, LVF);
	probs = 
		// HIGH         LOW          NORMAL        // HYP   LVF   
		  (0.05195448,  0.05076695,  0.8972785,    // FALSE FALSE 
		   0.0131168,   0.9750156,   0.01186758,   // FALSE TRUE  
		   0.9131112,   0.008948255, 0.07794061,   // TRUE  FALSE 
		   0.01298701,  0.9480519,   0.03896104);  // TRUE  TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (870, 72);
		height = 25;
		};
	};

node CVP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (LVV);
	probs = 
		// HIGH         LOW          NORMAL        // LVV    
		  (0.7069784,   0.01238283,  0.2806388,    // HIGH   
		   0.007076139, 0.9479196,   0.04500424,   // LOW    
		   0.006718161, 0.04012933,  0.9531525);   // NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (174, 222);
		height = 1;
		};
	};

node PCWP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (LVV);
	probs = 
		// HIGH         LOW          NORMAL        // LVV    
		  (0.94908,     0.009836825, 0.04108321,   // HIGH   
		   0.01160487,  0.9535805,   0.0348146,    // LOW    
		   0.009161128, 0.03969822,  0.9511406);   // NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 366);
		height = 2;
		};
	};

node STKV {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (HYP, LVF);
	probs = 
		// HIGH         LOW          NORMAL        // HYP   LVF   
		  (0.05261422,  0.04977734,  0.8976085,    // FALSE FALSE 
		   0.008119925, 0.9512805,   0.04059963,   // FALSE TRUE  
		   0.007651407, 0.4851511,   0.5071975,    // TRUE  FALSE 
		   0.007792208, 0.9844156,   0.007792208); // TRUE  TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (522, 660);
		height = 26;
		};
	};

node HIST {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = (LVF);
	probs = 
		// FALSE        TRUE          // LVF   
		  (0.9890853,   0.01091473,   // FALSE 
		   0.1074672,   0.8925328);   // TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (174, 510);
		height = 3;
		};
	};

node APL {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.9898755,   0.01012449);
	whenchanged = 0;
	visual V1 {
		center = (1218, 510);
		height = 19;
		};
	};

node TPR {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (APL);
	probs = 
		// HIGH         LOW          NORMAL        // APL   
		  (0.3012955,   0.2998308,   0.3988737,    // FALSE 
		   0.01228501,  0.980344,    0.007371007); // TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 510);
		height = 4;
		};
	};

node PMB {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.9897255,   0.01027449);
	whenchanged = 0;
	visual V1 {
		center = (1218, 660);
		height = 21;
		};
	};

node INT {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (ESOPHAGEAL, NORMAL, ONESIDED);
	parents = ();
	probs = 
		// ESOPHAGEAL   NORMAL       ONESIDED     
		  (0.03032272,  0.919756,    0.04992126);
	whenchanged = 0;
	visual V1 {
		center = (870, 804);
		height = 22;
		};
	};

node SHNT {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, NORMAL);
	parents = (PMB, INT);
	probs = 
		// HIGH         NORMAL        // PMB   INT        
		  (0.05074875,  0.9492512,    // FALSE ESOPHAGEAL 
		   0.04940136,  0.9505987,    // FALSE NORMAL     
		   0.9570273,   0.0429727,    // FALSE ONESIDED   
		   0.7857143,   0.2142857,    // TRUE  ESOPHAGEAL 
		   0.8973684,   0.1026316,    // TRUE  NORMAL     
		   0.9545454,   0.04545455);  // TRUE  ONESIDED   ;
	whenchanged = 0;
	visual V1 {
		center = (174, 948);
		height = 31;
		};
	};

node FIO2 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (LOW, NORMAL);
	parents = ();
	probs = 
		// LOW          NORMAL       
		  (0.05057247,  0.9494275);
	whenchanged = 0;
	visual V1 {
		center = (522, 948);
		height = 12;
		};
	};

node KINK {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.9636268,   0.03637318);
	whenchanged = 0;
	visual V1 {
		center = (1218, 1242);
		height = 23;
		};
	};

node VTUB {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = ();
	probs = 
		// HIGH         LOW          NORMAL       ZERO         
		  (0.05856914,  0.7333017,   0.01647335,  0.1916558);
	whenchanged = 0;
	visual V1 {
		center = (174, 1680);
		height = 36;
		};
	};

node VLNG {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (INT, KINK, VTUB);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // INT        KINK  VTUB   
		  (0.01470588,  0.04411765,  0.9264706,   0.01470588,   // ESOPHAGEAL FALSE HIGH   
		   0.01322115,  0.4723558,   0.008413462, 0.5060096,    // ESOPHAGEAL FALSE LOW    
		   0.05555556,  0.05555556,  0.1666667,   0.7222222,    // ESOPHAGEAL FALSE NORMAL 
		   0.02016129,  0.02822581,  0.01209677,  0.9395161,    // ESOPHAGEAL FALSE ZERO   
		   0.25,        0.08333334,  0.08333334,  0.5833333,    // ESOPHAGEAL TRUE  HIGH   
		   0.02,        0.02,        0.02,        0.94,         // ESOPHAGEAL TRUE  LOW    
		   0.25,        0.25,        0.25,        0.25,         // ESOPHAGEAL TRUE  NORMAL 
		   0.08333334,  0.08333334,  0.08333334,  0.75,         // ESOPHAGEAL TRUE  ZERO   
		   0.01214772,  0.9684159,   0.01214772,  0.00728863,   // NORMAL     FALSE HIGH   
		   0.01039748,  0.03004144,  0.01001381,  0.9495473,    // NORMAL     FALSE LOW    
		   0.9642857,   0.01870748,  0.008503402, 0.008503402,  // NORMAL     FALSE NORMAL 
		   0.01048125,  0.670062,    0.01284322,  0.3066135,    // NORMAL     FALSE ZERO   
		   0.01111111,  0.01111111,  0.01111111,  0.9666666,    // NORMAL     TRUE  HIGH   
		   0.0157563,   0.01155462,  0.009453782, 0.9632353,    // NORMAL     TRUE  LOW    
		   0.03333334,  0.03333334,  0.03333334,  0.9,          // NORMAL     TRUE  NORMAL 
		   0.0261194,   0.003731343, 0.01119403,  0.9589552,    // NORMAL     TRUE  ZERO   
		   0.9166667,   0.0530303,   0.007575758, 0.02272727,   // ONESIDED   FALSE HIGH   
		   0.01207386,  0.6612216,   0.006392045, 0.3203125,    // ONESIDED   FALSE LOW    
		   0.02777778,  0.08333334,  0.02777778,  0.8611111,    // ONESIDED   FALSE NORMAL 
		   0.002717391, 0.008152174, 0.008152174, 0.9809783,    // ONESIDED   FALSE ZERO   
		   0.1666667,   0.1666667,   0.1666667,   0.5,          // ONESIDED   TRUE  HIGH   
		   0.01923077,  0.01923077,  0.01923077,  0.9423077,    // ONESIDED   TRUE  LOW    
		   0.1666667,   0.1666667,   0.5,         0.1666667,    // ONESIDED   TRUE  NORMAL 
		   0.15,        0.45,        0.05,        0.35);        // ONESIDED   TRUE  ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (870, 1680);
		height = 35;
		};
	};

node VALV {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (INT, VLNG);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // INT        VLNG   
		  (0.05,        0.75,        0.15,        0.05,         // ESOPHAGEAL HIGH   
		   0.02227723,  0.002475247, 0.007425743, 0.9678218,    // ESOPHAGEAL LOW    
		   0.9078947,   0.03947368,  0.01315789,  0.03947368,   // ESOPHAGEAL NORMAL 
		   0.006868132, 0.9793956,   0.006868132, 0.006868132,  // ESOPHAGEAL ZERO   
		   0.01362683,  0.942348,    0.00524109,  0.03878407,   // NORMAL     HIGH   
		   0.9704198,   0.01185932,  0.009678299, 0.00804253,   // NORMAL     LOW    
		   0.01804124,  0.007731959, 0.9458763,   0.02835052,   // NORMAL     NORMAL 
		   0.01041593,  0.01070032,  0.01077142,  0.9681123,    // NORMAL     ZERO   
		   0.007042253, 0.7957746,   0.1619718,   0.03521127,   // ONESIDED   HIGH   
		   0.007352941, 0.9779412,   0.007352941, 0.007352941,  // ONESIDED   LOW    
		   0.0625,      0.0625,      0.0625,      0.8125,       // ONESIDED   NORMAL 
		   0.005543237, 0.009977827, 0.9678492,   0.01662971);  // ONESIDED   ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (870, 1824);
		height = 34;
		};
	};

node PVS {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (FIO2, VALV);
	probs = 
		// HIGH         LOW          NORMAL        // FIO2   VALV   
		  (0.05121294,  0.01347709,  0.9353099,    // LOW    HIGH   
		   0.02994012,  0.9281437,   0.04191617,   // LOW    LOW    
		   0.01298701,  0.974026,    0.01298701,   // LOW    NORMAL 
		   7.047216e-4, 0.9985905,   7.047216e-4,  // LOW    ZERO   
		   0.9787502,   0.01272194,  0.008527891,  // NORMAL HIGH   
		   0.01343438,  0.9469514,   0.03961419,   // NORMAL LOW    
		   0.01203293,  0.9366688,   0.05129829,   // NORMAL NORMAL 
		   3.794346e-5, 0.9893758,   0.01058623);  // NORMAL ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (174, 1974);
		height = 32;
		};
	};

node SAO2 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (SHNT, PVS);
	probs = 
		// HIGH         LOW          NORMAL        // SHNT   PVS    
		  (0.01123596,  0.6539326,   0.3348314,    // HIGH   HIGH   
		   0.007577884, 0.9814763,   0.01094583,   // HIGH   LOW    
		   0.008264462, 0.9669421,   0.02479339,   // HIGH   NORMAL 
		   0.9810212,   0.01009188,  0.00888688,   // NORMAL HIGH   
		   0.01021734,  0.9787929,   0.01098978,   // NORMAL LOW    
		   0.01690507,  0.01170351,  0.9713914);   // NORMAL NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1914, 1974);
		height = 11;
		};
	};

node ACO2 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (VALV);
	probs = 
		// HIGH         LOW          NORMAL        // VALV   
		  (0.00890839,  0.9058636,   0.08522803,   // HIGH   
		   0.01532442,  0.8845778,   0.1000978,    // LOW    
		   0.001814882, 0.07441016,  0.923775,     // NORMAL 
		   0.01004645,  0.8921897,   0.09776385);  // ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (174, 2262);
		height = 33;
		};
	};

node CCHL {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, NORMAL);
	parents = (TPR, SAO2, ACO2);
	probs = 
		// HIGH         NORMAL        // TPR    SAO2   ACO2   
		  (0.6428571,   0.3571429,    // HIGH   HIGH   HIGH   
		   0.05372341,  0.9462766,    // HIGH   HIGH   LOW    
		   0.015625,    0.984375,     // HIGH   HIGH   NORMAL 
		   0.9528302,   0.04716981,   // HIGH   LOW    HIGH   
		   0.301263,    0.698737,     // HIGH   LOW    LOW    
		   0.2973602,   0.7026398,    // HIGH   LOW    NORMAL 
		   0.5,         0.5,          // HIGH   NORMAL HIGH   
		   0.04941861,  0.9505814,    // HIGH   NORMAL LOW    
		   0.1,         0.9,          // HIGH   NORMAL NORMAL 
		   0.9444444,   0.05555556,   // LOW    HIGH   HIGH   
		   0.963388,    0.03661202,   // LOW    HIGH   LOW    
		   0.9725274,   0.02747253,   // LOW    HIGH   NORMAL 
		   0.990566,    0.009433962,  // LOW    LOW    HIGH   
		   0.988121,    0.01187905,   // LOW    LOW    LOW    
		   0.9906609,   0.00933908,   // LOW    LOW    NORMAL 
		   0.75,        0.25,         // LOW    NORMAL HIGH   
		   0.9763158,   0.02368421,   // LOW    NORMAL LOW    
		   0.9761904,   0.02380952,   // LOW    NORMAL NORMAL 
		   0.9642857,   0.03571429,   // NORMAL HIGH   HIGH   
		   0.950773,    0.04922701,   // NORMAL HIGH   LOW    
		   0.9730769,   0.02692308,   // NORMAL HIGH   NORMAL 
		   0.9919355,   0.008064516,  // NORMAL LOW    HIGH   
		   0.9603619,   0.03963813,   // NORMAL LOW    LOW    
		   0.9483159,   0.05168409,   // NORMAL LOW    NORMAL 
		   0.875,       0.125,        // NORMAL NORMAL HIGH   
		   0.9447116,   0.05528846,   // NORMAL NORMAL LOW    
		   0.8552632,   0.1447368);   // NORMAL NORMAL NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 2412);
		height = 27;
		};
	};

node HR {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (CCHL);
	probs = 
		// HIGH         LOW          NORMAL        // CCHL   
		  (0.8964138,   0.009816559, 0.09376962,   // HIGH   
		   0.04399549,  0.04891806,  0.9070864);   // NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1914, 2412);
		height = 29;
		};
	};

node CO {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (STKV, HR);
	probs = 
		// HIGH         LOW          NORMAL        // STKV   HR     
		  (0.9762511,   0.01272265,  0.01102629,   // HIGH   HIGH   
		   0.05263158,  0.2631579,   0.6842105,    // HIGH   LOW    
		   0.6855346,   0.01048218,  0.3039832,    // HIGH   NORMAL 
		   0.007973829, 0.7984052,   0.1936209,    // LOW    HIGH   
		   0.007633588, 0.9847328,   0.007633588,  // LOW    LOW    
		   0.008899298, 0.9569086,   0.03419204,   // LOW    NORMAL 
		   0.9479685,   0.009633732, 0.04239773,   // NORMAL HIGH   
		   0.00792393,  0.9619651,   0.03011093,   // NORMAL LOW    
		   0.01068635,  0.03756748,  0.9517462);   // NORMAL NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (522, 2556);
		height = 6;
		};
	};

node BP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (TPR, CO);
	probs = 
		// HIGH         LOW          NORMAL        // TPR    CO     
		  (0.8971179,   0.01276906,  0.0901131,    // HIGH   HIGH   
		   0.09273743,  0.2878957,   0.6193668,    // HIGH   LOW    
		   0.5421206,   0.04925579,  0.4086236,    // HIGH   NORMAL 
		   0.01090142,  0.898019,    0.09107959,   // LOW    HIGH   
		   0.008461921, 0.9880537,   0.003484321,  // LOW    LOW    
		   0.007475561, 0.9850489,   0.007475561,  // LOW    NORMAL 
		   0.7444351,   0.04852379,  0.2070411,    // NORMAL HIGH   
		   0.01755755,  0.9734686,   0.008973858,  // NORMAL LOW    
		   0.05229541,  0.1017964,   0.8459082);   // NORMAL NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 2556);
		height = 5;
		};
	};

node ERLO {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.9498775,   0.05012249);
	whenchanged = 0;
	visual V1 {
		center = (1914, 2556);
		height = 28;
		};
	};

node HRBP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (ERLO, HR);
	probs = 
		// HIGH         LOW          NORMAL        // ERLO  HR     
		  (0.9789924,   0.00989267,  0.01111493,   // FALSE HIGH   
		   0.01740295,  0.3708166,   0.6117805,    // FALSE LOW    
		   0.01056243,  0.9806807,   0.008756883,  // FALSE NORMAL 
		   0.003641661, 0.01092498,  0.9854333,    // TRUE  HIGH   
		   0.03225806,  0.9354839,   0.03225806,   // TRUE  LOW    
		   0.2873563,   0.2413793,   0.4712644);   // TRUE  NORMAL ;
	whenchanged = 0;
	visual V1 {
		center = (1218, 3138);
		height = 7;
		};
	};

node ERCA {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.90058,     0.09942);
	whenchanged = 0;
	visual V1 {
		center = (1914, 3138);
		height = 30;
		};
	};

node HREK {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (HR, ERCA);
	probs = 
		// HIGH         LOW          NORMAL        // HR     ERCA  
		  (0.9812104,   0.007540019, 0.01124955,   // HIGH   FALSE 
		   0.005448602, 0.006901562, 0.9876499,    // HIGH   TRUE  
		   0.297668,    0.3662551,   0.3360768,    // LOW    FALSE 
		   0.3877551,   0.3877551,   0.2244898,    // LOW    TRUE  
		   0.01094925,  0.9792441,   0.009806721,  // NORMAL FALSE 
		   0.3609467,   0.3271344,   0.3119189);   // NORMAL TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 3288);
		height = 8;
		};
	};

node HRSA {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (HR, ERCA);
	probs = 
		// HIGH         LOW          NORMAL        // HR     ERCA  
		  (0.9817749,   0.009233499, 0.008991573,  // HIGH   FALSE 
		   0.01053396,  0.01053396,  0.9789321,    // HIGH   TRUE  
		   0.3388203,   0.3415638,   0.3196159,    // LOW    FALSE 
		   0.3877551,   0.3877551,   0.2244898,    // LOW    TRUE  
		   0.0122822,   0.9763877,   0.0113301,    // NORMAL FALSE 
		   0.3440406,   0.3254438,   0.3305157);   // NORMAL TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (1914, 3288);
		height = 9;
		};
	};

node ECO2 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (ACO2, VLNG);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // ACO2   VLNG   
		  (0.75,        0.05,        0.15,        0.05,         // HIGH   HIGH   
		   0.0106383,   0.0106383,   0.9468085,   0.03191489,   // HIGH   LOW    
		   0.25,        0.25,        0.25,        0.25,         // HIGH   NORMAL 
		   0.003448276, 0.9344828,   0.03103448,  0.03103448,   // HIGH   ZERO   
		   0.9641393,   0.003073771, 0.02561475,  0.007172131,  // LOW    HIGH   
		   0.009363776, 0.9688302,   0.009107235, 0.01269882,   // LOW    LOW    
		   0.007936508, 0.007936508, 0.9603174,   0.02380952,   // LOW    NORMAL 
		   0.009201253, 0.00880971,  0.01092404,  0.971065,     // LOW    ZERO   
		   0.9083334,   0.025,       0.04166667,  0.025,        // NORMAL HIGH   
		   0.006234414, 0.01371571,  0.006234414, 0.9738154,    // NORMAL LOW    
		   0.008571428, 0.01428571,  0.9571428,   0.02,         // NORMAL NORMAL 
		   0.01043257,  0.9620865,   0.01348601,  0.01399491);  // NORMAL ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (522, 510);
		height = 14;
		};
	};

node PAP {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (PMB);
	probs = 
		// HIGH         LOW          NORMAL        // PMB   
		  (0.04993307,  0.05094335,  0.8991236,    // FALSE 
		   0.7917675,   0.02179177,  0.1864407);   // TRUE  ;
	whenchanged = 0;
	visual V1 {
		center = (1218, 72);
		height = 10;
		};
	};

node PRSS {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (INT, KINK, VTUB);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // INT        KINK  VTUB   
		  (0.6029412,   0.01470588,  0.3676471,   0.01470588,   // ESOPHAGEAL FALSE HIGH   
		   0.890625,    0.008413462, 0.09254808,  0.008413462,  // ESOPHAGEAL FALSE LOW    
		   0.05555556,  0.05555556,  0.05555556,  0.8333333,    // ESOPHAGEAL FALSE NORMAL 
		   0.004032258, 0.02016129,  0.02016129,  0.9556451,    // ESOPHAGEAL FALSE ZERO   
		   0.08333334,  0.5833333,   0.08333334,  0.25,         // ESOPHAGEAL TRUE  HIGH   
		   0.62,        0.06,        0.3,         0.02,         // ESOPHAGEAL TRUE  LOW    
		   0.25,        0.25,        0.25,        0.25,         // ESOPHAGEAL TRUE  NORMAL 
		   0.4166667,   0.25,        0.25,        0.08333334,   // ESOPHAGEAL TRUE  ZERO   
		   0.006316812, 0.9052478,   0.08017493,  0.008260447,  // NORMAL     FALSE HIGH   
		   0.4072667,   0.2914748,   0.2904006,   0.01085789,   // NORMAL     FALSE LOW    
		   0.9676871,   0.01530612,  0.01190476,  0.005102041,  // NORMAL     FALSE NORMAL 
		   0.9744612,   0.009300265, 0.00811928,  0.00811928,   // NORMAL     FALSE ZERO   
		   0.03333334,  0.5444444,   0.05555556,  0.3666667,    // NORMAL     TRUE  HIGH   
		   0.4359244,   0.2615546,   0.269958,    0.03256303,   // NORMAL     TRUE  LOW    
		   0.1,         0.03333334,  0.03333334,  0.8333333,    // NORMAL     TRUE  NORMAL 
		   0.01119403,  0.01865672,  0.01865672,  0.9514925,    // NORMAL     TRUE  ZERO   
		   0.9772727,   0.007575758, 0.007575758, 0.007575758,  // ONESIDED   FALSE HIGH   
		   0.9694602,   0.0078125,   0.01349432,  0.009232954,  // ONESIDED   FALSE LOW    
		   0.08333334,  0.6388889,   0.02777778,  0.25,         // ONESIDED   FALSE NORMAL 
		   0.008152174, 0.8342391,   0.04076087,  0.1168478,    // ONESIDED   FALSE ZERO   
		   0.1666667,   0.1666667,   0.1666667,   0.5,          // ONESIDED   TRUE  HIGH   
		   0.01923077,  0.01923077,  0.01923077,  0.9423077,    // ONESIDED   TRUE  LOW    
		   0.1666667,   0.1666667,   0.5,         0.1666667,    // ONESIDED   TRUE  NORMAL 
		   0.85,        0.05,        0.05,        0.05);        // ONESIDED   TRUE  ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (870, 222);
		height = 13;
		};
	};

node MINV {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (INT, VLNG);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // INT        VLNG   
		  (0.05,        0.05,        0.85,        0.05,         // ESOPHAGEAL HIGH   
		   0.01732673,  0.002475247, 0.007425743, 0.9727723,    // ESOPHAGEAL LOW    
		   0.03947368,  0.5657895,   0.01315789,  0.381579,     // ESOPHAGEAL NORMAL 
		   0.009615385, 0.9766483,   0.004120879, 0.009615385,  // ESOPHAGEAL ZERO   
		   0.0115304,   0.9654088,   0.009433962, 0.01362683,   // NORMAL     HIGH   
		   0.9685115,   0.009133043, 0.0115867,   0.01076881,   // NORMAL     LOW    
		   0.0128866,   0.4561856,   0.007731959, 0.5231959,    // NORMAL     NORMAL 
		   0.00977604,  0.009633843, 0.01041593,  0.9701742,    // NORMAL     ZERO   
		   0.9507042,   0.007042253, 0.02112676,  0.02112676,   // ONESIDED   HIGH   
		   0.01155462,  0.3539916,   0.007352941, 0.6271008,    // ONESIDED   LOW    
		   0.0625,      0.0625,      0.1875,      0.6875,       // ONESIDED   NORMAL 
		   0.01441242,  0.005543237, 0.9678492,   0.01219512);  // ONESIDED   ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (174, 660);
		height = 15;
		};
	};

node VMCH {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL, ZERO);
	parents = (VTUB);
	probs = 
		// HIGH         LOW          NORMAL       ZERO          // VTUB   
		  (0.8435635,   0.007246377, 0.1410912,   0.008098892,  // HIGH   
		   7.839662e-4, 7.839662e-4, 0.9975799,   8.521372e-4,  // LOW    
		   0.4516616,   0.01661631,  0.4969788,   0.0347432,    // NORMAL 
		   0.004302477, 0.3002608,   0.448631,    0.2468057);   // ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (1914, 660);
		height = 37;
		};
	};

node DISC {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = (VTUB, VMCH);
	probs = 
		// FALSE        TRUE          // VTUB   VMCH   
		  (0.9984848,   0.001515151,  // HIGH   HIGH   
		   0.9444444,   0.05555556,   // HIGH   LOW    
		   0.8945783,   0.1054217,    // HIGH   NORMAL 
		   0.85,        0.15,         // HIGH   ZERO   
		   0.9583333,   0.04166667,   // LOW    HIGH   
		   0.875,       0.125,        // LOW    LOW    
		   0.9990091,   9.908433e-4,  // LOW    NORMAL 
		   0.7307692,   0.2692308,    // LOW    ZERO   
		   0.07666667,  0.9233333,    // NORMAL HIGH   
		   0.75,        0.25,         // NORMAL LOW    
		   0.9181818,   0.08181818,   // NORMAL NORMAL 
		   0.7916667,   0.2083333,    // NORMAL ZERO   
		   0.8529412,   0.1470588,    // ZERO   HIGH   
		   0.8936632,   0.1063368,    // ZERO   LOW    
		   0.08861127,  0.9113888,    // ZERO   NORMAL 
		   0.904435,    0.09556494);  // ZERO   ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (1566, 1242);
		height = 24;
		};
	};

node MVS {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (HIGH, LOW, NORMAL);
	parents = (VMCH);
	probs = 
		// HIGH         LOW          NORMAL        // VMCH   
		  (0.831976,    0.009001286, 0.1590227,    // HIGH   
		   0.004674883, 0.8376541,   0.1576711,    // LOW    
		   7.492432e-4, 3.29667e-4,  0.9989211,    // NORMAL 
		   0.04951506,  0.04236855,  0.9081164);   // ZERO   ;
	whenchanged = 0;
	visual V1 {
		center = (522, 804);
		height = 16;
		};
	};

node ANES {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (FALSE, TRUE);
	parents = ();
	probs = 
		// FALSE        TRUE         
		  (0.8953303,   0.1046698);
	whenchanged = 0;
	visual V1 {
		center = (522, 1098);
		height = 20;
		};
	};
};
