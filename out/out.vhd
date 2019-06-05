library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity out is
port
(
    D : inout signed (15 downto 0);
    MIO : in bit;
	outPORT : out signed (15 downto 0)
);
end entity;
 
architecture rtl of busint is
begin
	process(D, MIO)
		variable VoutPORT: signed(15 downto 0);
	begin
		D <= "ZZZZZZZZZZZZZZZZ";
		
        if(MIO='1') then VoutPORT <= D; end if;
 
        outPORT <= VoutPORT;
	end process;
end rtl;