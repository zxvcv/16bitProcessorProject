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
			mD <= ram_block(to_integer(mADR(4 downto 0)));
		else
			mD <= "ZZZZZZZZZZZZZZZZ";
		end if;
		
		if(mWR = '1') then 
			ram_block(to_integer(mADR(4 downto 0))) <= mD;
		end if;
	end process;

	
	
end rtl;