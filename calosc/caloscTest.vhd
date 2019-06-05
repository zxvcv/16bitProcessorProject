library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity caloscTest is
	port (
		inputClk : in bit;
		inputINT : in std_logic;
		inputReset : in std_logic;
		outputPORT0 : out bit_vector (0 to 6);
		outputPORT1 : out bit_vector (0 to 6);
		outputPORT2 : out bit_vector (0 to 6);
		outputPORT3 : out bit_vector (0 to 6)
    );
end caloscTest;
 
architecture behaviour of caloscTest is
	
	--component begin
	component dekoder is
		port (
			i : in bit_vector(3 downto 0);
			o : out bit_vector(0 to 6)
		);
	end component;

	component control is
	port(
		clk : in bit;
		IR : in signed(15 downto 0);
		reset, C, Z, S, INT : in std_logic;
		Salu, Sbb, Sbc, Sba : out bit_vector(3 downto 0);
		Sid : out bit_vector(2 downto 0);
		Sa : out bit_vector(1 downto 0);
		LDF, Smar, Smbr, WR, RD, INTA, MIO : out bit
	);
	end component;

	component ALU is
	port (
		  A : in signed(15 downto 0);
		  B : in signed(15 downto 0);
		  Salu : in bit_vector(3 downto 0);
		  LDF : in bit;
		  clk : in bit;
		  Y : out signed (15 downto 0);
		  C,Z,S : out std_logic
		);
	end component;

	component Rejestry is
    port
    (
        clk : in bit;
        DI : in signed (15 downto 0);
        BA : in signed (15 downto 0);
		Sba : in bit_vector (3 downto 0);
        Sbb : in bit_vector (3 downto 0);
        Sbc : in bit_vector (3 downto 0);
        Sid : in bit_vector (2 downto 0);
        Sa : in bit_vector (1 downto 0);
        BB : out signed (15 downto 0);
        BC : out signed (15 downto 0);
        ADR : out signed (31 downto 0);
        IRout : out signed (15 downto 0)
    );
	end component;
	
	component busint is
		port
		(
			ADR : in signed(31 downto 0);
			DO : in signed(15 downto 0);
			Smar, Smbr, WRin, RDin, MIO : in bit;
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
	
	component outPORT is
	port
	(
		D : inout signed (15 downto 0);
		MIO : in bit;
		outP : out signed (15 downto 0)
	);
	end component;
	--component end

	--signal s_ADR : signed(31 downto 0);
	signal s_BA : signed(15 downto 0);
	signal s_BB : signed(15 downto 0);
	signal s_BC : signed(15 downto 0);
	signal s_ADR : signed(31 downto 0);
	signal s_DI : signed(15 downto 0);
	signal s_ADRmem : signed(31 downto 0);
	signal s_D : signed(15 downto 0);
	signal s_IR : signed (15 downto 0);
	signal s_PORT : signed(15 downto 0);
	
	signal s_Salu : bit_vector (3 downto 0);
	signal s_Sba : bit_vector (3 downto 0);
    signal s_Sbb : bit_vector (3 downto 0);
    signal s_Sbc : bit_vector (3 downto 0);
    signal s_Sid : bit_vector (2 downto 0);
    signal s_Sa : bit_vector (1 downto 0);
	signal s_WR : bit;
	signal s_RD : bit;
	signal s_Smar : bit;
	signal s_Smbr : bit;
	signal s_WRmem : bit;
	signal s_RDmem : bit;
	signal s_LDF : bit;
	signal s_S : std_logic;
	signal s_C : std_logic;
	signal s_Z : std_logic;
	
	signal s_INTA : bit;
	signal s_MIO : bit;
	
	signal s_out_PORT0 : bit_vector (3 downto 0);
	signal s_out_PORT1 : bit_vector (3 downto 0);
	signal s_out_PORT2 : bit_vector (3 downto 0);
	signal s_out_PORT3 : bit_vector (3 downto 0);
	
begin
	
	s_out_PORT0 <= to_bitvector(std_logic_vector(s_PORT(3 downto 0)));
	s_out_PORT1 <= to_bitvector(std_logic_vector(s_PORT(7 downto 4)));
	s_out_PORT2 <= to_bitvector(std_logic_vector(s_PORT(11 downto 8)));
	s_out_PORT3 <= to_bitvector(std_logic_vector(s_PORT(15 downto 12)));
	
	GateCTRL : control 
		port map ( 
			clk => inputClk,
			IR => s_IR,
			reset => inputReset,
			C => s_C,
			Z => s_Z,
			S => s_S,
			INT => inputINT,
			Salu => s_Salu,
			Sbb => s_Sbb,
			Sbc => s_Sbc,
			Sba => s_Sba,
			Sid => s_Sid,
			Sa => s_Sa,
			LDF => s_LDF,
			Smar => s_Smar,
			Smbr => s_Smbr,
			WR => s_WR,
			RD => s_RD,
			INTA => s_INTA,
			MIO => s_MIO
		);

	GateALU : ALU 
		port map ( 
			A => s_BB,
			B => s_BC,
			Salu => s_Salu,
			LDF => s_LDF,
			clk => inputClk,
			Y => s_BA,
			C => s_C,
			Z => s_Z,
			S => s_S
		);
	
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
			IRout => s_IR
		);
	
	GateBusInt : busint 
		port map (
			ADR => s_ADR,
			DO => s_BA,
			Smar => s_Smar,
			Smbr => s_Smbr,
			WRin => s_WR,
			RDin => s_RD,
			MIO => s_MIO,
			AD => s_ADRmem,
			D => s_D,
			DI => s_DI,
			WR => s_WRmem,
			RD => s_RDmem
		);
	
	GateMem : memory
		port map (
			mADR => s_ADRmem,
			mD => s_D,
			mWR => s_WRmem,
			mRD => s_RDmem
		);
	
	GatePORT : outPORT
		port map (
			D => s_D,
			MIO => s_MIO,
			outP => s_PORT
		);
	
	GateHexPORT0 : dekoder
		port map (
			i => s_out_PORT0,
			o => outputPORT0
		);
		
	GateHexPORT1 : dekoder
		port map (
			i => s_out_PORT1,
			o => outputPORT1
		);
	
	GateHexPORT2 : dekoder
		port map (
			i => s_out_PORT2,
			o => outputPORT2
		);
		
	GateHexPORT3 : dekoder
		port map (
			i => s_out_PORT3,
			o => outputPORT3
		);
		
end behaviour;