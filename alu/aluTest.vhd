library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity aluTest is
	port (
		inputA : in signed(2 downto 0);
		inputB : in signed(2 downto 0);
		inputSalu : in bit_vector(3 downto 0);
		inputLDF : in bit;
		inputClk : in bit;
			
		hexA : out bit_vector(0 to 6);
		hexB : out bit_vector(0 to 6);
		hexY : out bit_vector(0 to 6);
		outputC : out std_logic;
		outputZ : out std_logic;
		outputS : out std_logic
   );
end aluTest;
 
architecture behaviour of aluTest is
	
	--component begin
	component dekoder is
		port (
			i : in bit_vector(3 downto 0);
			o : out bit_vector(0 to 6)
		);
	end component;

	component ALU is
		port (
			A : in signed(15 downto 0);
			B : in signed(15 downto 0);
			Salu : in bit_vector (3 downto 0);
			LDF : in bit;
			clk : in bit;
			Y : out signed (15 downto 0);
			C,Z,S : out std_logic
		);
	end component;
	--component end

	signal inA : signed(15 downto 0);
	signal inB : signed(15 downto 0);
	signal Yout : signed (15 downto 0);
	signal AgateIN : bit_vector(3 downto 0);
	signal BgateIN : bit_vector(3 downto 0);
	signal YgateIN : bit_vector(3 downto 0);
	
   begin
   
   --jednostka ALU
	GateALU : ALU port map ( A => inA, B => inB, Salu => inputSalu, LDF => inputLDF, clk => inputClk,
							Y => Yout, C => outputC, Z => outputZ, S => outputS);

   --dekorery hex wyswietlajace pierwsze z czterech bajtow danych
	GateA : dekoder port map (i => AgateIN, o => hexA);
	GateB : dekoder port map (i => BgateIN, o => hexB);
	GateY : dekoder port map (i => YgateIN, o => hexY);
	
	--inA <= "0000000000000000";
	inA(15 downto 13) <= inputA;
	--inB <= "0000000000000000";
	inB(15 downto 13) <= inputB;
	
	AgateIN <= to_bitvector(std_logic_vector(inA(15 downto 12)));
	BgateIN <= to_bitvector(std_logic_vector(inB(15 downto 12)));
	YgateIN <= to_bitvector(std_logic_vector(Yout(15 downto 12)));
	
   end behaviour;