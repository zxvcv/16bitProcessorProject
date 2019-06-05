library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity control is
port(
    clk : in bit;
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
m29, m30, m31, m32, m33, m34, m35, m36, m37, m38, m40, m41, m50, m51, m52, m53, m60, m61, m62, m63, m64, m65, m66, m67, m68,
m70, m71, m72, m73, m74, m75, m76, m77, m78, m79, m80, m81, m82, m9, m91, m92, m93, m94);

signal state : state_type;
signal s_IR : bit_vector(15 downto 0);

begin

s_IR <= to_bitvector(std_logic_vector(IR));

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
							when others => 
								state <= m0;
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
						state <= m50; --JMP
					when "100" => --argument1 w R, argument2 w nastepnym słowie
						state <= m60; --odczyt st16
					when "101" => --argument1 w R, argument2 w nastepnych 2 słowach
						state <= m70; --odczyt st32l
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
				if(IR(12 downto 11) = "00") then state <= m41; 
				else if(IR(12 downto 11) = "01" and C='1') then state <= m41; 
				else if(IR(12 downto 11) = "10" and Z='1') then state <= m41; 
				else if(IR(12 downto 11) = "11" and S='1') then state <= m41; 
				else if (INT='1') then state <= m9; 
				else state <= m0;
				end if; end if; end if; end if; end if;
			when m41 => --JMP; JC; JZ; JS; (2/2)
				if(INT = '1') then state <= m9;
				else state <= m0;
				end if;
			when m50 => state <= m51; --JMP (1/4)
			when m51 => state <= m52; --JMP (2/4)
			when m52 => state <= m53; --JMP (3/4)
			when m53 => --JMP (4/4)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m60 => --wybor komendy
				case IR(12 downto 10) is
					when "000" => state <= m61; --MOV R, st16
					when "001" => state <= m62; --MOV R, st(R)
					when "010" => state <= m63; --ADD R, st16
					when "011" => state <= m64; --SUB R, st16
					when "100" => state <= m65; --CMP R, st16
					when "101" => state <= m66; --AND R, st16
					when "110" => state <= m67; --OR R, st16
					when "111" => state <= m68; --XOR R, st16
					when others => state <= m0;
				end case;
			when m70 => state <= m71; --odczyt st32h
			when m71 => state <= m72; --odczyt MEM(st32)
			when m72 => --wybor komendy
				case IR(12 downto 9) is
					when "0000" => state <= m73; --MOV R, MEM(st32)
					when "0001" => state <= m74; --MOV MEM(st32), R
					when "0010" => state <= m75; --ADD R, MEM(st32)
					when "0011" => state <= m76; --SUB R, MEM(st32)
					when "0100" => state <= m77; --CMP R, MEM(st32)
					when "0101" => state <= m78; --AND R, MEM(st32)
					when "0110" => state <= m79; --OR R, MEM(st32)
					when "0111" => state <= m80; --XOR R, MEM(st32)
					when "1000" => state <= m81; --IN
					when "1001" => state <= m82; --OUT
					when others => state <= m0;
				end case;
			when m61 => --MOV R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m62 => --MOV R, st(R)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m63 => --ADD R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m64 => --SUB R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m65 => --ADD R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m66 => --AND R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m67 => --OR R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m68 => --XOR R, st16
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
				
			when m73 => --MOV R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m74 => --MOV MEM(st32), R
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m75 => --ADD R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m76 => --SUB R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m77 => --CMP R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m78 => --AND R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m79 => --OR R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m80 => --XOR R, MEM(st32)
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m81 => --IN
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m82 => --OUT
				if INT = '1' then state <= m9;
				else state <= m0;
				end if;
			when m9 => state <= m9;				
			when others => state <= m0;
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
			MIO <= '0'; INTA <= '0';
			
        when m10 => --WAIT
			--
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';

        when m11 => --CALL (1/4)
			-- MAR <- SP; MBR <- PCh; SP <- SP - 1;
			
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= PCh, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= "1010"; Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m12 => --CALL (2/4)
			-- MAR <- SP; MBR <- PCl; SP <- SP - 1;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= PCl, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= "1011"; Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
        
		when m13 => --CALL (3/4)
			-- PCh <- ADh
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := PCh, BB <= ADh, BC <= DI, ADR <= AD
			Sba <= "1010"; Sbb <= "1000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m14 => --CALL (4/4)
			-- PCl <- ADl
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := PCl, BB <= ADl, BC <= DI, ADR <= AD
			Sba <= "1011"; Sbb <= "1001"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m15 => --RET (1/3)
			-- SP <- SP + 1
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, SP++, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m16 => --RET (2/3)
			-- MAR <- SP; PCl <- MBR; SP <- SP + 1;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCl := BA, BB <= DI, BC <= DI, SP++, ADR <= SP
			Sba <= "1011"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m17 => --RET (3/3)
			-- MAR <- SP; PCh <- MBR;
		
			--ALU: powtarzanie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCh := BA, BB <= DI, BC <= DI, ADR <= SP
			Sba <= "1010"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m20 => --PUSH R
			-- MEM(SP) <- R; SP--;
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= R, BC <= DI, SP--, ADR <= SP
			Sba <= "0000"; Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "011"; Sa <= "10";
			--BUSINT: MAR := ADR, D <= DO
			Smar <= '1'; Smbr <= '1'; WR <= '1'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m21 => --POP R (1/2)
			-- SP++;
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, SP++, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "010"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m22 => --POP R (2/2)
			-- R <- MEM(SP);
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := R, BB <= DI, BC <= DI, ADR <= SP
			Sba <= s_IR(3 downto 0); Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "10";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m23 => --NEG R
			-- R <- (-R);
		
			--ALU: Y <= -BB, brak bitow
			Salu <= "1001"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m24 => --NOT R
			-- R <- (~R);
		
			--ALU: Y <= not BB, brak bitow
			Salu <= "1000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m25 => --DEC R
			--  R <- R--;
		
			--????????????
			--nie da sie w jednym cyklu, mozne podmienic rozkaz "0001" Alu na AA - 1
		
		when m26 => --INC R
			-- R <- R++;
		
			--ALU: Y <= BB++, brak bitow
			Salu <= "1101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m27 => --SHR R
			-- R <- R>>1;
		
			--ALU: Y <= BB>>1, brak bitow
			Salu <= "1111"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m28 => --SHL R
			-- R <- R<<1;
		
			--ALU: Y <= BB<<1, brak bitow
			Salu <= "1110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m29 => --MOV R, RM
			--  R <- RM
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= RM, BC <= DI, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(3 downto 0); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m30 => --MOV RM, R
			-- RM <- R
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: RM := BA, BB <= R, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(7 downto 4); Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m31 => --ADD R, RM
			-- R <- R + RM
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m32 => --SUB R, RM
			-- R <- R - RM
		
			--ALU: Y <= BB - BC, brak bitow
			Salu <= "0011"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m33 => --CMP R, RM
			-- R - RM; (ustawiane sa bity LDF)
		
			--ALU: Y <= BB - BC, ustawiane sa bity
			Salu <= "0011"; LDF <= '1';
			--REJESTRY: TMP := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= "0001"; Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m34 => --AND R, RM
			-- R <= R && RM;
		
			--ALU: Y <= BB && BC, brak bitow
			Salu <= "0101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
		
		when m35 => --OR R, RM
			-- R <= R || RM;
		
			--ALU: Y <= BB || BC, brak bitow
			Salu <= "0100"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m36 => --XOR R, RM
			-- R <= R xor RM;
		
			--ALU: Y <= BB xor BC, brak bitow
			Salu <= "0110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= RM, ADR <= AD
			Sba <= s_IR(7 downto 4); Sbb <= s_IR(7 downto 4); Sbc <= s_IR(3 downto 0); Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
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
			MIO <= '0'; INTA <= '0';
			
		when m41 => --JMP; JC; JZ; JS; (2/2)
			-- PC <- PC + st16
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '1';
			--REJESTRY: PCl := BA, BB <= PCl, BC <= TMP, ADR <= AD
			Sba <= "1011"; Sbb <= "1011"; Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m50 => --JMP (1/4)
			-- odczyt st32 (1/2)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: ATMPl := BA, BB <= DI, BC <= DI, PC++, ADR <= PC
			Sba <= "1111"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "001"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m51 => --JMP (2/4)
			-- odczyt st32 (2/2)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: ATMPh := BA, BB <= DI, BC <= DI, ADR <= PC
			Sba <= "1110"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m52 => --JMP (3/4)
			-- PC <- st32 (1/2)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCl := BA, BB <= ATMPl, BC <= DI, ADR <= AD
			Sba <= "1011"; Sbb <= "1111"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m53 => --JMP (4/4)
			-- PC <- st32 (2/2)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: PCh := BA, BB <= ATMPh, BC <= DI, ADR <= AD
			Sba <= "1010"; Sbb <= "1110"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m60 => --przygotownie do komendy z argumentami R i st16
			-- odczyt st16
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: TMP := BA, BB <= DI, BC <= DI, PC++, ADR <= PC
			Sba <= "0001"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "001"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '1'; INTA <= '0';
			
		when m61 => --MOV R, st16
			-- R <- st16
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= TMP, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= "0001"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m62 => --MOV R, st(R)
			-- 
		
			--???
			--co to jest st(R)
			
		when m63 => --ADD R, st16
			-- R <- R + st16
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m64 => --SUB R, st16
			-- R <- R - st16
		
			--ALU: Y <= BB - BC, brak bitow
			Salu <= "0011"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m65 => --CMP R, st16
			-- R - st16; (ustawiane sa bity LDF)
		
			--ALU: Y <= BB - BC, ustawiane sa bity
			Salu <= "0011"; LDF <= '1';
			--REJESTRY: TMP := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= "0001"; Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m66 => --AND R, st16
			-- R <= R && st16;
		
			--ALU: Y <= BB && BC, brak bitow
			Salu <= "0101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m67 => --OR R, st16
			-- R <= R || st16;
		
			--ALU: Y <= BB || BC, brak bitow
			Salu <= "0100"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m68 => --XOR R, st16
			-- R <= R xor st16;
		
			--ALU: Y <= BB xor BC, brak bitow
			Salu <= "0110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m70 => --przygotownie do komendy z argumentami R i st32 (1/3)
			-- odczyt st32l
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: ATMPl := BA, BB <= DI, BC <= DI, PC++, ADR <= PC
			Sba <= "1111"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "001"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m71 => --przygotownie do komendy z argumentami R i st32 (2/3)
			-- odczyt st32h
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: ATMPh := BA, BB <= DI, BC <= DI, ADR <= PC
			Sba <= "1110"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "01";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m72 => --przygotownie do komendy z argumentami R i st32 (3/3)
			-- odczyt MEM(st32)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: TMP := BA, BB <= DI, BC <= DI, ADR <= ATMP
			Sba <= "0001"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "11";
			--BUSINT: AD <= ADR, DI <= D
			Smar <= '1'; Smbr <= '0'; WR <= '0'; RD <= '1';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m73 => --MOV R, MEM(st32)
			--  R <- MEM(st32)
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: R := BA, BB <= TMP, BC <= DI, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= "0001"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m74 => --MOV MEM(st32), R
			-- 
		
			--?????????
			
		when m75 => --ADD R, MEM(st32)
			-- R <- R + MEM(st32)
		
			--ALU: Y <= BB + BC, brak bitow
			Salu <= "0010"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m76 => --SUB R, MEM(st32)
			-- R <- R - MEM(st32)
		
			--ALU: Y <= BB - BC, brak bitow
			Salu <= "0011"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m77 => --CMP R, MEM(st32)
			-- R - MEM(st32); (ustawiane sa bity LDF)
		
			--ALU: Y <= BB - BC, ustawiane sa bity
			Salu <= "0011"; LDF <= '1';
			--REJESTRY: TMP := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= "0001"; Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m78 => --AND R, MEM(st32)
			-- R <= R && MEM(st32);
		
			--ALU: Y <= BB && BC, brak bitow
			Salu <= "0101"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m79 => --OR R, MEM(st32)
			-- R <= R || MEM(st32);
		
			--ALU: Y <= BB || BC, brak bitow
			Salu <= "0100"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m80 => --XOR R, MEM(st32)
			-- R <= R xor MEM(st32);
		
			--ALU: Y <= BB xor BC, brak bitow
			Salu <= "0110"; LDF <= '0';
			--REJESTRY: R := BA, BB <= R, BC <= TMP, ADR <= AD
			Sba <= s_IR(3 downto 0); Sbb <= s_IR(3 downto 0); Sbc <= "0001"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
			
		when m81 => --IN
			-- 
		
			--????????????
			
		when m82 => --OUT
			-- 
		
			--????????????
		
        when others =>
			-- nothing
		
			--ALU: powtarznie BB, brak bitow
			Salu <= "0000"; LDF <= '0';
			--REJESTRY: IR := BA, BB <= DI, BC <= DI, ADR <= AD
			Sba <= "0000"; Sbb <= "0000"; Sbc <= "0000"; Sid <= "000"; Sa <= "00";
			--BUSINT: bez zmian, D w stanie "Z"
			Smar <= '0'; Smbr <= '0'; WR <= '0'; RD <= '0';
			--??
			MIO <= '0'; INTA <= '0';
	    
    end case;
end process;

end rtl;