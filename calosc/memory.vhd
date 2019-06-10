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
		ram_block(0) <=  "1000000000000010"; --MOV A, 0
		ram_block(1) <=  "0000000000000000"; 
		ram_block(2) <=  "1000000000000011"; --MOV B, 1
		ram_block(3) <=  "0000000000000001";
		ram_block(4) <=  "0011000100000010"; --OUT A
		ram_block(5) <=  "0011000100000011"; --OUT B
		ram_block(6) <=  "0010100001000010"; --MOV C, A
		ram_block(7) <=  "0010101001000011"; --ADD C, B 
		ram_block(8) <=  "0010100000100011"; --MOV A, B
		ram_block(9) <=  "0010100000110100"; --MOV B, C
		ram_block(10) <= "0110000000000000"; --JMP MEM(5)
		ram_block(11) <= "0000000000000101"; 
		ram_block(12) <= "0000000000000000";
		--ram_block(13) <= "0000000000000000";
		--ram_block(14) <= "0011000100000000"; 
		--ram_block(15) <=
		--ram_block(16) <=
		--ram_block(17) <=
		--ram_block(18) <=
		--ram_block(19) <=
		--ram_block(20) <=
		--ram_block(21) <=
		--ram_block(22) <=
		--ram_block(23) <=
		--ram_block(24) <=
		--ram_block(25) <=
		--ram_block(26) <=
		--ram_block(27) <=
		--ram_block(28) <=
		--ram_block(29) <=
		--ram_block(30) <=
		--ram_block(31) <=
	
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