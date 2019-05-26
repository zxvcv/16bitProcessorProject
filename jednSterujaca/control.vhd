library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity control is
port(
    clk : in std_logic;
    IR : in signed(15 downto 0);
    reset, C, Z, S, INT : in std_logic;
    Salu, Sbb, Sbc, Sba : out bit_vector(3 downto 0);
    Sid : out bit_vector(2 downto 0);
    Sa : out bit_vector(1 downto 0);
    LDF, Smar, Smbr, WR, RD, INTA, MIO : out bit
);
end entity;
 
architecture rtl of control is

type state_type is (m0, m1, m10, m11, m12, m13, m14, m15, m16, m17, m20, m21, m22, m23, m24, m25, m26, m27, m28,
m29, m30, m31, m32, m33, m34, m35, m36, m37, m38, m40, m41, m50, m60, m61, m62, m63, m64, m65, m66, m67, m68,
m69, m70, m80, m81, m82, m83, m84, m9, m91, m92, m93, m94);

signal state : state_type;

begin

process (clk, reset)
begin
    if reset = '1' then
        state <= m0;
    elsif (clk'event and clk='1') then
        case state is
            when m0=> 
                state <= m1;
            when m1=>
                case IR(15 downto 13) is --wykrywnie typu rozkazu
					when "000" => --bez argumentow
						case IR(12 downto 11) is
							when "00" => --NOP
								if(INT='0') then state <= m0;
								else state <= m9;
								end if;
							when "01" => state <= m10; --WAIT
							when "10" => state <= m11; --CALL
							when "11" => state <= m15; --RET
						end case;
					when "001" => --argumenty w R
						case IR(12 downto 8) is
							when "00000" => state <= m20; --PUSH
							when "00001" => state <= m21; --POP
							when "00010" => state <= m23; --NEG
							when "00011" => state <= m24; --NOT
							when "00100" => state <= m25; --DEC
							when "00101" => state <= m26; --INC
							when "00110" => state <= m27; --SHR
							when "00111" => state <= m28; --SHL
							when "01000" => state <= m29; --MOV R, M
							when "01001" => state <= m30; --MOV M, R
							when "01010" => state <= m31; --ADD
							when "01011" => state <= m32; --SUB
							when "01100" => state <= m33; --CMP
							when "01101" => state <= m34; --AND
							when "01110" => state <= m35; --OR
							when "01111" => state <= m36; --XOR
							when "10000" => state <= m37; --IN
							when "10001" => state <= m38; --OUT
							when others => state <= m0;
						end case;
					when "010" => --argument st16 w nastepnym  słowie
						state <= m40;
					when "011" => --???
						state <= m50;
					when "100" => --argument1 w R, argument2 w nastepnym słowie
						state <= m60;
					when "101" => --argument1 w R, argument2 w nastepnych 2 słowach
						state <= m80;
					when others => 
						state <= m0;
				end case;
			when m10=> --WAIT
				if INT = '1' then state <= m9;
				else state <= m10;
				end if;
			when m11 => state <= m12; --CALL (1/4)
			when m12 => state <= m13; --CALL (2/4)
			when m13 => state <= m14; --CALL (3/4)
			when m14 =>	--CALL (4/4)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m15 => state <= m16; --RET (1/3)
			when m16 => state <= m17; --RET (2/3)
			when m17 => --RET (3/3)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			...
        end case;
    end if;
end process;
 
process (state)
begin
    case state is
        when m0 => --POBIERANIE ROZKAZU (Fetch)
			--
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, PC++, ADR <= PC
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "001"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
        when m1 => --DEKODOWANIE ROZKAZU
			--
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
        when m10 => --WAIT
			--
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';

        when m11 => --CALL (1/4)
			-- MAR <- SP; MBR <- PCh; SP <- SP - 1;
			
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= PCh, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= "1010"; Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m12 => --CALL (2/4)
			-- MAR <- SP; MBR <- PCl; SP <- SP - 1;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= PCl, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= "1011"; Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
        
		when m13 => --CALL (3/4)
			-- PCh <- ADh
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := PCh, BB <= ADh, BC <= DI, ADR <= AD
			Sba <= "1010"; Sbb <= "1000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m14 => --CALL (4/4)
			-- PCl <- ADl
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := PCl, BB <= ADl, BC <= DI, ADR <= AD
			Sba <= "1011"; Sbb <= "1001"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m15 => --RET (1/3)
			-- SP <- SP + 1
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, SP++, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m16 => --RET (2/3)
			-- MAR <- SP; PCl <- MBR; SP <- SP + 1;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCl := BA, BB <= DI, BC <= DI, SP++, ADR <= SP
			Sba <= "1011"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m17 => --RET (3/3)
			-- MAR <- SP; PCh <- MBR
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCh := BA, BB <= DI, BC <= DI, ADR <= SP
			Sba <= "1010"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
			
			
			
		...
        when others =>
			-- nothing
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
	    
    end case;
end process;

end rtl;