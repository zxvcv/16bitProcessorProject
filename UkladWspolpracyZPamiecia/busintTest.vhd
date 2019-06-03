library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity busintTest is
	port (
			inputADR : in signed(2 downto 0);
			inputDO : in signed(2 downto 0);
			inputSmar : in bit;
			inputSmbr : in bit;
			inputWRin : in bit;
			inputRDin : in bit;

			outputDI : out bit_vector (0 to 6);
			outputWR : out bit;
			outputRD : out bit;
			
			--inputy na hexy
			outputADR : out bit_vector(0 to 6);
			outputDO : out bit_vector(0 to 6)
    );
end busintTest;
 
architecture behaviour of busintTest is
	
	--component begin
	component dekoder is
		port (
			i : in bit_vector(3 downto 0);
			o : out bit_vector(0 to 6)
		);
	end component;

	component busint is
		port
		(
			ADR : in signed(31 downto 0);
			DO : in signed(15 downto 0);
			Smar, Smbr, WRin, RDin : in bit;
			AD : out signed (31 downto 0);
			D : inout signed (15 downto 0);
			DI : out signed(15 downto 0);
			WR, RD : out bit
		);
	end component;
	
	component memory is
	port
	(
		mADR : in signed(31 downto 0);
		mD : inout signed (15 downto 0);
		mWR, mRD : in bit
	);
	end component;
	--component end

	signal s_ADR : signed(31 downto 0); --wszystkie bity na 0 oprocz 2 najmniej znaczacych (slidery)
	signal s_DO : signed(15 downto 0); --wszystkie bity na 0 oprocz 2 najmniej znaczacych (slidery)
	signal s_AD : signed (31 downto 0);
	signal s_D : signed (15 downto 0);
	signal s_DI : signed(15 downto 0);
	signal s_WR : bit;
	signal s_RD : bit;
	
	signal s_out_ADR : bit_vector (3 downto 0);
	signal s_out_DO : bit_vector (3 downto 0);
	signal s_out_DI : bit_vector (3 downto 0);
	
	
begin
   
    s_ADR (31 downto 3) <= "00000000000000000000000000000"; s_ADR (2 downto 0) <= inputADR;
	s_DO (15 downto 3) <= "0000000000000"; s_DO (2 downto 0) <= inputDO;
	
	--wyjsiowe sygnaly na hex
	s_out_ADR <= to_bitvector(std_logic_vector(s_ADR (3 downto 0)));
	s_out_DO <= to_bitvector(std_logic_vector(s_DO (3 downto 0)));
	s_out_DI <= to_bitvector(std_logic_vector(s_DI (3 downto 0)));
	
	outputRD <= s_RD;
	outputWR <= s_WR;
	
	
	--jednostka Rejestry
	GateRej : busint 
		port map (
			ADR => s_ADR,
			DO => s_DO,
			Smar => inputSmar,
			Smbr => inputSmbr,
			WRin => inputWRin,
			RDin => inputRDin,
			AD => s_AD,
			D => s_D,
			DI => s_DI,
			WR => s_WR,
			RD => s_RD
		);
	
	GateMem : memory
		port map (
			mADR => s_AD,
			mD => s_D,
			mWR => s_WR,
			mRD => s_RD
		);
	
	GateHexADR : dekoder
		port map (
			i => s_out_ADR,
			o => outputADR
		);
		
	GateHexDO : dekoder
		port map (
			i => s_out_DO,
			o => outputDO
		);
		
	GateHexDI: dekoder
		port map (
			i => s_out_DI,
			o => outputDI
		);
	
	--process(s_WR, s_RD)
	--begin
	--	if(s_RD='1') then s_D (15 downto 2) <= "00000000000000"; s_D (1 downto 0) <= inputD; end if;
	--	if(s_WR='1') then s_D (15 downto 0) <= "ZZZZZZZZZZZZZZZZ"; end if;
	--end process;
	
end behaviour;