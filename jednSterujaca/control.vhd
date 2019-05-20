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
              case IR(15 downto 13) is
                 when "000" =>
                    case IR(12 downto 11) is
                      when "00" =>
                        if(INT='0') then state <= m0;
                        else state <= m9;
                        end if;
                      when "01" => state <= m10;
                      when "10" => state <= m11;
                      when "11" => state <= m15;
                    end case;
                 when "001" =>
                    case IR(12 downto 8) is
                           when "00000" => state <= m20;
                           when "00001" => state <= m21;
                           when "00010" => state <= m23;
                           when "00011" => state <= m24;
                           when "00100" => state <= m25;
                           when "00101" => state <= m26;
                           when "00110" => state <= m27;
                           when "00111" => state <= m28;
                           when "01000" => state <= m29;
                           when "01001" => state <= m30;
                           when "01010" => state <= m31;
                           when "01011" => state <= m32;
                           when "01100" => state <= m33;
                           when "01101" => state <= m34;
                           when "01110" => state <= m35;
                           when "01111" => state <= m36;
                           when "10000" => state <= m37;
                           when "10001" => state <= m38;
                           when others => state <= m0;
                     end case;
                 when "010" => state <= m40;
                 when "011" => state <= m50;
                 when "100" => state <= m60;
                 when "101" => state <= m80;
                 when others => state <= m0;
              end case;
           when m10=>
              if INT = '1' then state <= m9;
              else state <= m10;
              end if;
           when m11 => state <= m12;
           when m12 => state <= m13;
           when m13 => state <= m14;
           when m14 =>
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
        when m0 =>
           Sa <= "01"; Sbb <= "0000"; Sba <= "0000"; Sid <="001"; Sbc <="0000"; MIO <='1';
           Smar <='1'; Smbr <= '0'; WR <='0'; RD <='1'; Salu <="0000"; LDF <='0'; INTA <='0';
        when m1 =>
           Sa <= "00"; Sbb <= "0000"; Sba <= "0000"; Sid <="000"; Sbc <="0000"; MIO <='1';
           Smar <='0'; Smbr <= '0'; WR <='0'; RD <='0'; Salu <="0000"; LDF <='0'; INTA <='0';
        when m10 =>
           Sa <= "00"; Sbb <= "0000"; Sba <= "0000"; Sid <="000"; Sbc <="0000"; MIO <='1';
           Smar <='0'; Smbr <= '0'; WR <='0'; RD <='0'; Salu <="0000"; LDF <='0'; INTA <='0';
        when m11 =>
           Sa <= "10"; Sbb <= "1010"; Sba <= "0000"; Sid <="011"; Sbc <="0000"; MIO <='1';
           Smar <='1'; Smbr <= '1'; WR <='1'; RD <='0'; Salu <="0000"; LDF <='0'; INTA <='0';
        ...
        when others =>
           Sa <= "00"; Sbb <= "0000"; Sba <= "0000"; Sid <="000"; Sbc <="0000"; MIO <='1';
           Smar <='0'; Smbr <= '0'; WR <='0'; RD <='0'; Salu <="0000"; LDF <='0'; INTA <='0';
    end case;
end process;
end rtl;