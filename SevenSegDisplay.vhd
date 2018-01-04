----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    20:53:28 08/05/2012 
-- Design Name: 
-- Module Name:    SevenSegDisplay - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity SevenSegDisplay is
    Port ( clk: std_logic;
			  D : in  STD_LOGIC_VECTOR (15 downto 0) := "0001001101111111";
           Enable : in  STD_LOGIC;
           blank : in  STD_LOGIC;
           S : out  STD_LOGIC_VECTOR (7 downto 0);
           A : out  STD_LOGIC_VECTOR (3 downto 0));
end SevenSegDisplay;

architecture Behavioral of SevenSegDisplay is
constant zero: std_logic_vector(7 downto 0) := 		"11000000";
constant one: std_logic_vector(7 downto 0) := 		"11111001";
constant two: std_logic_vector(7 downto 0) := 		"10100100";
constant three: std_logic_vector(7 downto 0) := 	"10110000";
constant four: std_logic_vector(7 downto 0) := 		"10011001";
constant five: std_logic_vector(7 downto 0) := 		"10010010";
constant six: std_logic_vector(7 downto 0) := 		"10000010";
constant seven: std_logic_vector(7 downto 0) := 	"11111000";
constant eight: std_logic_vector(7 downto 0) := 	"10000000";
constant nine: std_logic_vector(7 downto 0) := 		"10011000";
constant ten: std_logic_vector(7 downto 0) := 		"10001000";

constant eleven: std_logic_vector(7 downto 0) := 	"10000011";
constant twelve: std_logic_vector(7 downto 0) := 	"11000110";
constant thirteen: std_logic_vector(7 downto 0) := "10100001";
constant fourteen: std_logic_vector(7 downto 0) := "10000110";
constant fifteen: std_logic_vector(7 downto 0) := 	"10001110";

type state_type is (sA, sB, sC, sD);
signal state_reg, state_next: state_type;
signal out_nibble: std_logic_vector(3 downto 0) := "0000";
signal dclk : std_logic := '0';
signal clockCounter: std_logic_vector(15 downto 0) := (others => '0');

begin
	dclk <= clockCounter(7);
	
	process(clk)
	begin
		if (clk'event and clk='1') then
			clockCounter <= clockCounter + 1;
		end if;
	end process;
	
	process(dclk)
	begin
		if (dclk'event and dclk='1') then
			state_reg <= state_next;
		end if;
	end process;
	
	process(state_reg)
	begin		
		state_next <= state_reg;
		
		case state_reg is
			when sA =>
				state_next <= sB;
			when sB =>
				state_next <= sC;
			when sC =>
				state_next <= sD;
			when sD =>
				state_next <= sA;
		end case;
	end process;
	
	process(state_reg)
	begin
		A <= "1111";
		out_nibble <= "0000";
		
		case state_reg is
		when sA =>
			A <= "1110";
			out_nibble <= D(3 downto 0);
		when sB =>
			A <= "1101";
			out_nibble <= D(7 downto 4);
		when sC =>
			A <= "1011";
			out_nibble <= D(11 downto 8);
		when sD =>
			A <= "0111";
			out_nibble <= D(15 downto 12);
		end case;
	end process;
	
	process(out_nibble)
	begin
		if out_nibble = "0000" then
			S <= zero;
		elsif out_nibble = "0001" then
			S <= one;
		elsif out_nibble = "0010" then
			S <= two;
		elsif out_nibble = "0011" then
			S <= three;
		elsif out_nibble = "0100" then
			S <= four;
		elsif out_nibble = "0101" then
			S <= five;
		elsif out_nibble = "0110" then
			S <= six;
		elsif out_nibble = "0111" then
			S <= seven;
		elsif out_nibble = "1000" then
			S <= eight;
		elsif out_nibble = "1001" then
			S <= nine;
		elsif out_nibble = "1010" then
			S <= ten;
		elsif out_nibble = "1011" then
			S <= eleven;
		elsif out_nibble = "1100" then
			S <= twelve;
		elsif out_nibble = "1101" then
			S <= thirteen;
		elsif out_nibble = "1110" then
			S <= fourteen;
		elsif out_nibble = "1111" then
			S <= fifteen;
		end if;
	end process;
end Behavioral;

