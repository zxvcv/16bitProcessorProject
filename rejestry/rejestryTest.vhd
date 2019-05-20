library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity rejestryTest is
	port (
		inputClk : in std_logic;
        inputDI : in signed (15 downto 0);
        inputBA : in signed (15 downto 0);
        inputSbb : in signed (3 downto 0);
        inputSbc : in signed (3 downto 0);
        inputSba : in signed (3 downto 0);
        inputSid : in signed (2 downto 0);
        inputSa : in signed (1 downto 0);
        
		outputBB : out signed (15 downto 0);
        outputBC : out signed (15 downto 0);
        outputADR : out signed (31 downto 0);
        outputIRout : out signed (15 downto 0)
    );
end rejestryTest;
 
architecture behaviour of aluTest is
	
	--component begin
--	component dekoder is
--		port (
--			i : in bit_vector(3 downto 0);
--			o : out bit_vector(0 to 6)
--		);
--	end component;

	component Rejestry is
		port
		(
			clk : in std_logic;
			DI : in signed (15 downto 0);
			BA : in signed (15 downto 0);
			Sbb : in signed (3 downto 0);
			Sbc : in signed (3 downto 0);
			Sba : in signed (3 downto 0);
			Sid : in signed (2 downto 0);
			Sa : in signed (1 downto 0);
			BB : out signed (15 downto 0);
			BC : out signed (15 downto 0);
			ADR : out signed (31 downto 0);
			IRout : out signed (15 downto 0)
		);
	end entity;
	--component end

	
   begin
   
	   --jednostka Rejestry
		GateRej : Rejestry port map ( clk => inputClk, DI => inputDI, BA => inputBA, Sbb => inputSbb, Sbc => inputSbc, Sba => inputSba,
									Sid => inputSid, Sa => inputSa, BB => outputBB, BC => outputBC, ADR => outputADR, IRout => outputIRout);

	
   end behaviour;