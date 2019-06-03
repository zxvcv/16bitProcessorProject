library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity memory is
port
(
    mADR : in signed(31 downto 0);
    mD : inout signed (15 downto 0);
    mWR, mRD : in bit
);
end memory;
 
 
 --brak ograniczenia adresu od góry (max 31)
architecture rtl of memory is
	type mem is array(0 to 31) of signed(15 downto 0);
	signal ram_block : mem;
	
begin

	process(mADR, mD, mWR, mRD)
	begin
		
		if(mRD = '1') then 
			case mADR(4 downto 0) is
				when "00000" => mD <= ram_block(0);
				when "00001" => mD <= ram_block(1);
				when "00010" => mD <= ram_block(2);
				when "00011" => mD <= ram_block(3);
				when others => mD <= ram_block(4);
			end case;

			--mD <= ram_block(to_integer(mADR(4 downto 0)));
		else
			mD <= "ZZZZZZZZZZZZZZZZ";
			
			if(mWR = '1') then 
				case mADR(4 downto 0) is
					when "00000" => ram_block(0) <= mD;
					when "00001" => ram_block(1) <= mD;
					when "00010" => ram_block(2) <= mD;
					when "00011" => ram_block(3) <= mD;
					when others => ram_block(4) <= mD;
				end case;
			else
				
			--ram_block(to_integer(mADR(4 downto 0))) <= mD; 
			end if;
			
		end if;
	end process;

	
	
end rtl;