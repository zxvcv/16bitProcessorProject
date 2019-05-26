library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ALU is
port (
      A : in signed(15 downto 0);
      B : in signed(15 downto 0);
      Salu : in bit_vector (3 downto 0);
      LDF : in bit;
      clk : in bit;
      Y : out signed (15 downto 0);
      C,Z,S : out std_logic
	);
end entity;
 
architecture rtl of ALU is

begin
	
  process (Salu, A, B, clk)
       variable res, AA, BB,CC: signed (16 downto 0);
       variable CF,ZF,SF : std_logic;
       begin
         AA(16) := A(15);
         AA(15 downto 0) := A;
         BB(16) := B(15);
         BB(15 downto 0) := B;
         CC(0) := CF;
         CC(16 downto 1) := "0000000000000000";
         case Salu is
             when "0000" => res := AA; 
             when "0001" => res := BB;
             when "0010" => res := AA + BB;
             when "0011" => res := AA - BB;
             when "0100" => res := AA or BB;
			 when "0101" => res := AA and BB;
			 when "0110" => res := AA xor BB;
			 when "0111" => res := AA xnor BB;
			 when "1000" => res := not AA;
			 when "1001" => res := -AA;
			 when "1010" => res := "00000000000000000";
			 when "1011" => res := AA + BB + CC;
			 when "1100" => res := AA - BB - CC;
			 when "1101" => res := AA + 1;
			 when "1110" => res := shift_left(AA, 1);
			 when "1111" => res := shift_right(AA, 1);
			 --res(15 downto 0) := AA(16 downto 1);
         end case;
         Y <= res(15 downto 0);
         Z <= ZF; --zero
         S <= SF; --parzystosc
         C <= CF; --przesuniece
         if (clk'event and clk='1') then
             if (LDF='1') then
                if (res = "00000000000000000") then 
					ZF:='1';
                else 
					ZF:='0';
				end if;
				
				if (res(15)='1') then 
					SF:='1';
				else 
					SF:='0'; 
				end if;
				
				CF := res(16) xor res(15);
             end if;
         end if;
  end process;
end rtl;
