%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% THIS PROGRAM TURNS TRANSITION MATRICES INTO GAMMA_INVERSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% change directory
%userpath ('FILL IN\Replication\results')

%% SELF 
%Gammasqtemp = xlsread('occgamma_mm_c_v22.xlsx','transmat_cnormssym','A1:V22');
%Gammatemp=sqrtm(Gammasqtemp);
%Gammatemp_inv=inv(Gammatemp)
%xlswrite('Gammainv_mm_cnormssym_v2.xlsx',Gammatemp_inv)

Gammasqtemp = xlsread('occgamma_mm_c_v22.xlsx','transmat_c','A1:V22');
Gammatemp=sqrtm(Gammasqtemp);
xlswrite('Gammamat_mm_c_v2.xlsx',Gammatemp)
Gammatemp_inv=inv(Gammatemp)
xlswrite('Gammainv_mm_c_v2.xlsx',Gammatemp_inv)
