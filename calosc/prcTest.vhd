library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity prcTest is
	port (
		inputClk : in bit;
		inputADR : in signed(31 downto 0); --na rysunku jest out w kodzie in?!
		
		inoutD : inout signed (15 downto 0);
		
		outputWR, outputRD : out bit
    );
end prcTest;
 
architecture behaviour of prcTest is
	
	component ALU is
		port (
			A : in signed(15 downto 0); 		--sygnal
			B : in signed(15 downto 0); 		--sygnal
			Salu : in bit_vector (3 downto 0); 		--uklSterujacy
			LDF : in bit;							--??	
			clk : in bit;						--WEJscie
			Y : out signed (15 downto 0); 		--sygnal
			C,Z,S : out std_logic					--??
		);
	end component;
	
	component Rejestry is
		port
		(
			clk : in std_logic;					--WEJscie
			DI : in signed (15 downto 0);		--sygnal
			BA : in signed (15 downto 0);		--sygnal
			Sbb : in signed (3 downto 0);			--uklSterujacy
			Sbc : in signed (3 downto 0);			--uklSterujacy
			Sba : in signed (3 downto 0);			--uklSterujacy
			Sid : in signed (2 downto 0);			--uklSterujacy
			Sa : in signed (1 downto 0);			--uklSterujacy
			BB : out signed (15 downto 0);		--sygnal
			BC : out signed (15 downto 0);		--sygnal
			ADR : out signed (31 downto 0);		--sygnal
			IRout : out signed (15 downto 0)		--??
		);
	end component;
	
	component busint is
		port
		(
			ADR : in signed(31 downto 0);		--WEJscie
			DO : in signed(15 downto 0);		--sygnal
			Smar, Smbr, WRin, RDin : in bit;		--uklSterujacy
			AD : out signed (31 downto 0);		--sygnal
			D : inout signed (15 downto 0);		--WEJscie/WYJscie
			DI : out signed(15 downto 0);		--sygnal
			WR, RD : out bit					--WYJscie
		);
	end component;
	--component end
	
	signal s_BB : signed(15 downto 0);
	signal s_BC : signed(15 downto 0);
	signal s_BA : signed(15 downto 0);
	signal s_DI : signed(15 downto 0);
	signal s_ADR : signed (31 downto 0);
	
   begin
   
   --jednostka ALU
	GateALU : ALU 
		port map ( 
			A => s_BB,
			B => s_BC,
			Salu => 	--uklSterujacy
			LDF => 
			clk => inputClk,
			Y => s_BA,
			C => 
			Z => 
			S => 
		);
		
	--jednostka rejestrow
	GateREJ : Rejestry 
		port map ( 
			clk => inputClk,
			DI => s_DI,
			BA => s_BA,
			Sbb =>		--uklSterujacy
			Sbc =>		--uklSterujacy
			Sba =>		--uklSterujacy
			Sid =>		--uklSterujacy
			Sa =>		--uklSterujacy
			BB => s_BB,
			BC => s_BC,
			ADR => s_ADR,
			IRout =>
		);

	--jednostka wspolpracy z pamiecia
	GateBUS : busint 
		port map ( 
			ADR => inputADR,
			DO => s_BA,
			Smar => 	--uklSterujacy
			Smbr =>		--uklSterujacy
			WRin =>		--uklSterujacy
			RDin =>		--uklSterujacy
			AD => s_ADR,
			D => inoutD,
			DI => s_DI,
			WR => outputWR,
			RD => outputRD
		);
	
   end behaviour;