library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity rejestryTest is
	port (
		inputClk : in std_logic;
        inputDI : in signed (1 downto 0);
        inputBA : in signed (1 downto 0);
		inputSba : in signed (1 downto 0);
        inputSbb : in signed (1 downto 0);
        
        inputSid : in signed (1 downto 0);
        
		outputBB : out bit_vector (0 to 6);
        outputBC : out bit_vector (0 to 6);
        outputADR : out bit_vector (0 to 6);
        outputIRout : out bit_vector (0 to 6);
		
		--dodatkowe wyjscia pokazujace tylko san wejsc
		outputDI : out bit_vector (0 to 6);
		outputBA : out bit_vector (0 to 6)
    );
end rejestryTest;
 
architecture behaviour of rejestryTest is
	
	--component begin
	component dekoder is
		port (
			i : in bit_vector(3 downto 0);
			o : out bit_vector(0 to 6)
		);
	end component;

	component Rejestry is
		port
		(
			clk : in std_logic;
			DI : in signed (15 downto 0);
			BA : in signed (15 downto 0);
			Sba : in signed (3 downto 0);
			Sbb : in signed (3 downto 0);
			Sbc : in signed (3 downto 0);
			Sid : in signed (2 downto 0);
			Sa : in signed (1 downto 0);
			BB : out signed (15 downto 0);
			BC : out signed (15 downto 0);
			ADR : out signed (31 downto 0);
			IRout : out signed (15 downto 0)
		);
	end component;
	--component end

	--signal s_inA : signed(15 downto 0);
	
    signal s_DI : signed (15 downto 0); --wszystkie bity na stale do 0 oprocz dwóch najmniej znaczacych
    signal s_BA : signed (15 downto 0); --wszystkie bity na stale do 0 oprocz dwóch najmniej znaczacych
    signal s_Sba : signed (3 downto 0); --pierwsze 2 bity na stale do 0 a pozostale 2 na slidery "00XX"
	signal s_Sbb : signed (3 downto 0); --pierwsze 2 bity na stale do 0 a pozostale 2 na slidery "00XX"
    signal s_Sbc : signed (3 downto 0); --na stale ustawia wartosc rejestru A na linie BC "0010"
    signal s_Sid : signed (2 downto 0); --na pierwszy bit stale ustawione 0 pozostale do sliderow "0XX"
    signal s_Sa : signed (1 downto 0); --na stale ustawione bity "01" (odczyt PC)
        
	signal s_BB : signed (15 downto 0); --najmniej znaczace 4 bity na hex
    signal s_BC : signed (15 downto 0); --najmniej znaczace 4 bity na hex
    signal s_ADR : signed (31 downto 0); --najmniej znaczace 4 bity na hex
    signal s_IRout : signed (15 downto 0); --najmniej znaczace 4 bity na hex
	
	signal s_out_BB : bit_vector (3 downto 0);
    signal s_out_BC : bit_vector (3 downto 0);
    signal s_out_ADR : bit_vector (3 downto 0);
    signal s_out_IRout : bit_vector (3 downto 0);
	signal s_out_DI : bit_vector (3 downto 0);
	signal s_out_BA : bit_vector (3 downto 0);
	
begin
   
    s_DI (15 downto 2) <= "00000000000000"; s_DI (1 downto 0) <= inputDI;
    s_BA (15 downto 2) <= "00000000000000"; s_BA (1 downto 0) <= inputBA;
	s_Sba (3 downto 2) <= "00";  s_Sba (1 downto 0) <=  inputSba;

	s_Sbb (3 downto 2) <= "00"; s_Sbb (1 downto 0) <= inputSbb;
    s_Sbc <= "0010";
    s_Sid (2) <= '0'; s_Sid (1 downto 0) <= inputSid;
	s_Sa <= "01";
   
	--wyjsiowe sygnaly na hex
	s_out_BB <= to_bitvector(std_logic_vector(s_BB (3 downto 0)));
	s_out_BC <= to_bitvector(std_logic_vector(s_BC (3 downto 0)));
	s_out_ADR <= to_bitvector(std_logic_vector(s_ADR (3 downto 0)));
	s_out_IRout <= to_bitvector(std_logic_vector(s_IRout (3 downto 0)));
	s_out_DI <= to_bitvector(std_logic_vector(s_DI (3 downto 0)));
	s_out_BA <= to_bitvector(std_logic_vector(s_BA (3 downto 0)));
	
	--jednostka Rejestry
	GateRej : Rejestry 
		port map ( 
			clk => inputClk,
			DI => s_DI,
			BA => s_BA,
			Sbb => s_Sbb, 
			Sbc => s_Sbc, 
			Sba => s_Sba,
			Sid => s_Sid,
			Sa => s_Sa,
			BB => s_BB,
			BC => s_BC,
			ADR => s_ADR,
			IRout => s_IRout
		);	

	GateHexBB : dekoder
		port map (
			i => s_out_BB,
			o => outputBB
		);
		
	GateHexBC : dekoder
		port map (
			i => s_out_BC,
			o => outputBC
		);
		
	GateHexADR : dekoder
		port map (
			i => s_out_ADR,
			o => outputADR
		);
		
	GateHexIRout : dekoder
		port map (
			i => s_out_IRout,
			o => outputIRout
		);
	
	GateHexDI : dekoder
		port map (
			i => s_out_DI,
			o => outputDI
		);
		
	GateHexBA : dekoder
		port map (
			i => s_out_BA,
			o => outputBA
		);
	
end behaviour;