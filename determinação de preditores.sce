clear;
clc;
mprintf("\n programa que determina o valor de H0 e Kt");
n=input("\n digíte o dia do ano: ");
delta=23.45*sin(2*(%pi)*((284+n)/365));
phi=-0.0523598775597;
mprintf("\n selicione os dados do pireliômetro");
dadospire=uigetfile("*. txt",pwd(), "escolha um arquivo");
B= fscanfMat(dadospire);
mprintf("\n selicione os tempos do piro heliômetro");
tempopire=uigetfile("*. txt",pwd(), "escolha um arquivo");
E= fscanfMat(tempopire);
for j=1:length(B) do
    var(j,1)=12-E(j,1);
    if var(j,1)>0 then
        dif(j,1)=-((var(j,1)*15)-((E(j,2)/60)*15)-((E(j,3)/3600)*15))*(1/360)*(%pi*2);
end
    if var(j,1)<=0 then
        dif(j,1)=(abs(var(j,1)*15)+((E(j,2)/60)*15)+((E(j,3)/3600)*15))*(1/360)*(%pi*2);
end
G0(j,1)=1367*(1+(0.033*cos((%pi*2)*(n/365))))*((cos(phi)*cos(delta*(%pi*2)*(1/360))*cos(dif(j,1)))+(sin(phi)*sin(delta*(%pi*2)*(1/360))));
Kt(j,1)=B(j,1)/G0(j,1);
end
filename=fullfile(TMPDIR,"data.csv");
csvWrite(G0,filename,ascii(9),".","%.2f");
mgetl(filename);
edit(filename);
