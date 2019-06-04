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
		ram_block(0) <= "1000000000000010"; --wpisanie do rejestru A wartosci (argument w nastepnym slowie operacja MOV
		ram_block(1) <= "0000000000000110"; --wartosc 6
		ram_block(2) <= "1000000000000011"; --wpisanie do rejestru A wartosci (argument w nastepnym slowie operacja MOV
		ram_block(3) <= "0000000000000011"; --wartosc 3
		ram_block(4) <= "0010101000100011"; --dodanie wartosci w rejestrach A i B (argumenty w IR, operacja ADD)
	
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