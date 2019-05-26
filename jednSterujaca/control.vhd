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
							when "00000" => state <= m20; --PUSH R
							when "00001" => state <= m21; --POP R
							when "00010" => state <= m23; --NEG R
							when "00011" => state <= m24; --NOT R
							when "00100" => state <= m25; --DEC R
							when "00101" => state <= m26; --INC R
							when "00110" => state <= m27; --SHR R
							when "00111" => state <= m28; --SHL R
							when "01000" => state <= m29; --MOV R, RM
							when "01001" => state <= m30; --MOV RM, R
							when "01010" => state <= m31; --ADD R, RM
							when "01011" => state <= m32; --SUB R, RM
							when "01100" => state <= m33; --CMP R, RM
							when "01101" => state <= m34; --AND R, RM
							when "01110" => state <= m35; --OR R, RM
							when "01111" => state <= m36; --XOR R, RM
							when "10000" => state <= m37; --IN R, IO(AD)
							when "10001" => state <= m38; --OUT IO(AD), R
							when others => state <= m0;
						end case;
					when "010" => --argument st16 w nastepnym  słowie
						state <= m40; --JMP; JC; JZ; JS;
					when "011" => --argument st32 w 2 nastepnych slowach
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
			when m20 => --PUSH R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m21 => state <= m22; --PUSH R (1/2)
			when m22 => --PUSH R (2/2)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m23 => --NEG R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m24 => --NOT R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m25 => --DEC R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m26 => --INC R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m27 => --SHR R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m28 => --SHL R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m29 => --MOV R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m30 => --MOV RM, R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m31 => --ADD R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m32 => --SUB R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m33 => --CMP R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m34 => --AND R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m35 => --OR R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m36 => --XOR R, RM
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m37 => --IN R, IO(AD)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m38 => --OUT IO(AD), R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m40 => --JMP; JC; JZ; JS; (1/2)
				if(IR(12 downto 11) = "00") state <= m41; 
				else if(IR(12 downto 11) = "01" and C) then state <= m41; 
				else if(IR(12 downto 11) = "10" and Z) then state <= m41; 
				else if(IR(12 downto 11) = "11" and S) then state <= m41; 
				else if (INT) then state <= m9; 
				else state <= m0;
				end if;
			when m41 => --JMP; JC; JZ; JS; (2/2)
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
			-- MAR <- SP; PCh <- MBR;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCh := BA, BB <= DI, BC <= DI, ADR <= SP
			Sba <= "1010"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m20 => --PUSH R
			-- MEM(SP) <- R; SP--;
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= R, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m21 => --POP R (1/2)
			-- SP++;
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, SP++, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m22 => --POP R (2/2)
			-- R <- MEM(SP);
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := R, BB <= DI, BC <= DI, ADR <= SP
			Sba <= IR(3 downto 0); Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m23 => --NEG R
			-- R <- (-R);
		
			--ALU: Y <= -BB, brak bitow
			Salu <= "1001"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m24 => --NOT R
			-- R <- (~R);
		
			--ALU: Y <= not BB, brak bitow
			Salu <= "1000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m25 => --DEC R
			--  R <- R--;
		
			--????????????
			--nie da sie w jednym cyklu, mozne podmienic rozkaz "0001" Alu na AA - 1
		
		when m26 => --INC R
			-- R <- R++;
		
			--ALU: Y <= BB++, brak bitow
			Salu <= "1101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m27 => --SHR R
			-- R <- R>>1;
		
			--ALU: Y <= BB>>1, brak bitow
			Salu <= "1111"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m28 => --SHL R
			-- R <- R<<1;
		
			--ALU: Y <= BB<<1, brak bitow
			Salu <= "1110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m29 => --MOV R, RM
			--  R <- RM
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= RM, BC <= DI, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m30 => --MOV RM, R
			-- RM <- R
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: RM := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= IR(3 downto 0); Sbb <= IR(7 downto 4); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m31 => --ADD R, RM
			-- R <- R + RM
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m32 => --SUB R, RM
			-- R <- R - RM
		
			--ALU: Y <= BB - BC, brak bitow
			Salu <= "0011"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m33 => --CMP R, RM
			-- R - RM; (ustawiane sa bity LDF)
		
			--ALU: Y <= BB - BC, ustawiane sa bity
			Salu <= "0011"; LDF <= '1';
			--REJESTRY: TMP := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= "0001"; Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m34 => --AND R, RM
			-- R <= R && RM;
		
			--ALU: Y <= BB && BC, brak bitow
			Salu <= "0101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
		
		when m35 => --OR R, RM
			-- R <= R || RM;
		
			--ALU: Y <= BB || BC, brak bitow
			Salu <= "0100"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m36 => --XOR R, RM
			-- R <= R xor RM;
		
			--ALU: Y <= BB xor BC, brak bitow
			Salu <= "0110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= IR(7 downto 4); Sbb <= IR(7 downto 4); Sbc <= IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m37 => --IN R, IO(AD)
			-- 
		
			--????????????
			
		when m38 => --OUT IO(AD), R
			-- 
		
			--????????????

		when m40 => --JMP; JC; JZ; JS; (1/2)
			-- odczyt st16
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: TMP := BA, BB <= DI, BC <= DI, PC++, ADR <= PC
			Sba <= "0001"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "001"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m41 => --JMP; JC; JZ; JS; (2/2)
			-- PC <- PC + st16
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '1';
			--REJESTRY: PCl := BA, BB <= PCl, BC <= TMP, ADR <= AD
			Sba <= "1011"; Sbb <= "1011"; Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
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