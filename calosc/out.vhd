library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity outPORT is
port
(
    D : inout signed (15 downto 0);
    MIO : in bit;
	outP : out signed (15 downto 0)
);
end entity;
 
architecture rtl of outPORT is
begin
	process(D, MIO)
		variable VoutP: signed(15 downto 0) := "0000000000000000";
	begin
		D <= "ZZZZZZZZZZZZZZZZ";
		
        if(MIO='1') then VoutP := D; end if;
 
        outP <= VoutP;
	end process;
end rtl;