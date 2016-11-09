// ~->[DNET-1]->~

// File created by someone at MonashU using Netica 5.15 on Nov 09, 2016 at 17:13:41.

bnet camml {
AutoCompile = TRUE;
autoupdate = TRUE;
whenchanged = 1478671825;

visual V1 {
	defdispform = BELIEFBARS;
	nodelabeling = TITLE;
	NodeMaxNumEntries = 50;
	nodefont = font {shape= "Arial"; size= 9;};
	linkfont = font {shape= "Arial"; size= 9;};
	windowposn = (25, 25, 1138, 502);
	resolution = 72;
	drawingbounds = (1080, 720);
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

node V1 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = ();
	probs = 
		// A            B            
		  (0.03164684,  0.9683532);
	whenchanged = 0;
	belief = (0.03164684, 0.9683532);
	visual V1 {
		center = (654, 186);
		height = 1;
		};
	};

node V9 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V1);
	probs = 
		// A            B             // V1 
		  (0.7681388,   0.2318612,    // A  
		   0.3644295,   0.6355705);   // B  ;
	whenchanged = 0;
	belief = (0.3772057, 0.6227943);
	visual V1 {
		center = (240, 426);
		height = 5;
		};
	};

node V3 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = ();
	probs = 
		// A            B            
		  (0.3348165,   0.6651835);
	whenchanged = 0;
	belief = (0.3348165, 0.6651835);
	visual V1 {
		center = (516, 426);
		height = 3;
		};
	};

node V10 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V3);
	probs = 
		// A            B             // V3 
		  (0.03627948,  0.9637205,    // A  
		   0.563881,    0.436119);    // B  ;
	whenchanged = 0;
	belief = (0.3872313, 0.6127687);
	visual V1 {
		center = (378, 26);
		height = 10;
		};
	};

node V12 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V9, V10);
	probs = 
		// A            B             // V9 V10 
		  (0.1063612,   0.8936388,    // A  A   
		   0.2921713,   0.7078287,    // A  B   
		   0.3824627,   0.6175373,    // B  A   
		   0.7342849,   0.265715);    // B  B   ;
	whenchanged = 0;
	belief = (0.4555293, 0.5444707);
	visual V1 {
		center = (618, 42);
		height = 12;
		};
	};

node V6 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V3);
	probs = 
		// A            B             // V3 
		  (0.07629143,  0.9237086,    // A  
		   0.5898843,   0.4101157);   // B  ;
	whenchanged = 0;
	belief = (0.4179249, 0.5820751);
	visual V1 {
		center = (378, 462);
		height = 4;
		};
	};

node V5 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = ();
	probs = 
		// A            B            
		  (0.05454455,  0.9454554);
	whenchanged = 0;
	belief = (0.05454455, 0.9454554);
	visual V1 {
		center = (102, 186);
		height = 7;
		};
	};

node V2 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V5);
	probs = 
		// A            B             // V5 
		  (0.07600733,  0.9239927,    // A  
		   0.245717,    0.754283);    // B  ;
	whenchanged = 0;
	belief = (0.2364603, 0.7635397);
	visual V1 {
		center = (618, 324);
		height = 2;
		};
	};

node V7 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V2);
	probs = 
		// A            B             // V2 
		  (0.102537,    0.897463,     // A  
		   0.578172,    0.4218279);   // B  ;
	whenchanged = 0;
	belief = (0.4657032, 0.5342968);
	visual V1 {
		center = (138, 48);
		height = 8;
		};
	};

node V11 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = (V5);
	probs = 
		// A            B             // V5 
		  (0.9551282,   0.0448718,    // A  
		   0.6629124,   0.3370876);   // B  ;
	whenchanged = 0;
	belief = (0.6788512, 0.3211488);
	visual V1 {
		center = (516, 26);
		height = 11;
		};
	};

node V4 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = ();
	probs = 
		// A            B            
		  (0.9526547,   0.04734527);
	whenchanged = 0;
	belief = (0.9526547, 0.04734527);
	visual V1 {
		center = (138, 324);
		height = 6;
		};
	};

node V8 {
	kind = NATURE;
	discrete = TRUE;
	chance = CHANCE;
	states = (A, B);
	parents = ();
	probs = 
		// A            B            
		  (0.02624737,  0.9737526);
	whenchanged = 0;
	belief = (0.02624737, 0.9737526);
	visual V1 {
		center = (240, 26);
		height = 9;
		};
	};
ElimOrder = (V4, V8, V1, V6, V7, V11, V3, V12, V9, V10, V5, V2);
};
