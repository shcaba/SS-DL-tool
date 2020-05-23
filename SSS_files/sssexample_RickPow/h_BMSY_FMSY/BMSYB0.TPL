DATA_SECTION
 init_int Amax;
 init_number M;
 init_vector S(0,Amax);
 init_vector Wght(0,Amax);
 init_vector Fec(0,Amax);

 init_int SSR;
 init_number FMSYOverM;
 number FMSY
 !! FMSY = FMSYOverM * M;
 init_number BMSYB0;
 init_number Par1;
 init_number Par2;
 
 
PARAMETER_SECTION
 init_number logC;
 init_bounded_number Steep2(-20,100);

 number FF;
 number SPR;
 number YPR;
 number SPRF0;
 number SPRFT;
 number YPRFT;
 number SPRF1;
 number YPRF1;
 number SPRF2;
 number YPRF2;
 number Power;
 number Steep;
 number Recs;
 number Rec2;
 number Penal;

 objective_function_value Obj;


// ====================================================================================================== 
// ====================================================================================================== 

PRELIMINARY_CALCS_SECTION 
 int Iage;
 
 // Compute SPR(F=0)
 FF = 0;
 GetYPR();
 SPRF0 = SPR;
 
 // Compute SPR/YPR (F=FMSY)
 FF = FMSY;
 GetYPR();
 SPRFT = SPR; YPRFT = YPR;
 
 // Compute stuff for derivatives
 FF = FMSY+0.001;
 GetYPR();
 SPRF1 = SPR; YPRF1 = YPR;
 FF = FMSY-0.001;
 GetYPR();
 SPRF2 = SPR; YPRF2 = YPR;
  
 logC = Par1; 
 Steep2 = Par2;
  
// ====================================================================================================== 
// ====================================================================================================== 

PROCEDURE_SECTION
 dvariable Hupper,Y1,Y2,Deriv,SSB,Yield;
 
 Penal = 0;
 cout << logC << " " << Steep2 << endl;
 
 Power = exp(logC);
 if (SSR == 1)
  {
   Hupper = 1.0/(5.0 * pow(0.2,Power));
   Steep = 0.2 + (Hupper - 0.2)/(1+exp(-1*Steep2))+1.0e-5;
  } 
 if (SSR == 2)
  {
   Hupper = 10.0;
   Steep = 0.2 + (Hupper - 0.2)/(1+exp(-1*Steep2))+1.0e-5;
  }
  
 
 SPR = SPRFT; GetRec(); 
 Yield = Recs * YPRFT;
 SSB = Recs * SPR/SPRF0*100;
 
 SPR = SPRF1; GetRec();
 Y1 = Recs * YPRF1;
 SPR = SPRF2; GetRec();
 Y2 = Recs * YPRF2;
 Deriv = (Y1-Y2)/0.002;
 
 Obj = 100*square(Deriv) + square(BMSYB0-SSB) + 100.0*Penal;
 cout << "A " << Power  << " " << Hupper << " " << Steep << " " << Deriv << " " << SSB << " " << Penal << " " << Obj << endl;
 
// ====================================================================================================== 

FUNCTION GetYPR
 int Iage;
 dvariable CAA;
 dvar_vector Z(0,Amax),N(0,Amax);
 
 // Specify Z
 for (Iage=0;Iage<=Amax;Iage++)
  { Z(Iage) = M + S(Iage)*FF; if (Z(Iage) > 0.999) Z(Iage) = 0.999; }
 
 // Specify N
 N(0) = 1;
 for (Iage=1;Iage<=Amax;Iage++) N(Iage) = N(Iage-1)*(1-Z(Iage-1));
 N(Amax) = N(Amax)/(1.0-Z(Amax));
 
 // Compute 
 YPR = 0; SPR = 0;
 for (Iage=0;Iage<=Amax;Iage++)
  {
   CAA = S(Iage)*FF*N(Iage);
   YPR += CAA*Wght(Iage);
   SPR += Fec(Iage)*N(Iage);
  }
 
// ====================================================================================================== 

FUNCTION GetRec
 dvariable Top,Bot,Ratio, Ratio2;

 // Shepherd
 if (SSR == 1)
  {
   Top = 5.0*Steep*(1.0-pow(0.2,Power))*SPR/SPRF0 - (1.0-5.0*Steep*pow(0.2,Power));
   Bot = (5.0*Steep - 1.0);
   Ratio2 = Top/Bot;
 
   Ratio = sqrt(square(Ratio2));
   Recs = pow(Ratio,1.0/Power)*SPRF0/SPR;
   if (Ratio2 < 0) Rec2 = 0; else Rec2 = Recs;
  } 
  
 // Generalized Ricker 
 if (SSR == 2)
  {
   Top =  pow(0.8,Power)*log(SPRF0/SPR)/log(5.0*Steep);
   Top = posfun(Top,0.000001,Penal);
   Recs = (SPRF0/SPR) * (1.0 - pow(Top,1.0/Power));
   Recs = posfun(Recs,0.0001,Penal);
   if (Recs < 0) Rec2 = 0; else Rec2 = Recs;
  }

// ====================================================================================================== 
// ====================================================================================================== 
 
REPORT_SECTION
 int II;
 dvariable Yield, SSB;
 report << Steep << " " << Power << endl;
 report << S << endl;
 report << Wght << endl;
 report << Fec << endl;

 report << endl;
 report << "F Yield SSB" << endl;
 for (II=1;II<=100;II++)
  {
   FF = float(II-1)*FMSY/33.0;
   GetYPR(); GetRec();
   Yield = Rec2 * YPR;
   SSB = Rec2 * SPR;
   report << FF << " " << Yield << " " << SSB/SPRF0*100 << endl;
  }


