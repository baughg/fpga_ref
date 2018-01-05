----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    17:53:14 01/04/2018 
-- Design Name: 
-- Module Name:    usbStream - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity usbStream is
    Port ( clk : in  STD_LOGIC;
           wclk : in  STD_LOGIC;
			  reset : in  STD_LOGIC;
			  start : in  STD_LOGIC;
           d0_ready : in  STD_LOGIC;
           d0 : in  STD_LOGIC_VECTOR (7 downto 0);
           d1_ready : in  STD_LOGIC;
           d1 : in  STD_LOGIC_VECTOR (7 downto 0);
           usb_send : out  STD_LOGIC;
			  out_test : out  STD_LOGIC;
           usb_data : out  STD_LOGIC_VECTOR (7 downto 0);
			  id : out  STD_LOGIC_VECTOR (7 downto 0);
           usb_ready : in  STD_LOGIC);
end usbStream;

architecture Behavioral of usbStream is
type ram_type is array(0 to 255) of std_logic_vector(7 downto 0);
signal d0_ram : ram_type := (others => (others => '0'));   --setup 256 bytes of ram
signal d1_ram : ram_type := (others => (others => '0'));   --setup 256 bytes of ram	
signal d0_bytes              : std_logic_vector(7 downto 0) := (others => '0');
signal d1_bytes              : std_logic_vector(7 downto 0) := (others => '0');
signal d0_bytes_read         : std_logic_vector(7 downto 0) := (others => '0');
signal d1_bytes_read         : std_logic_vector(7 downto 0) := (others => '0');
signal d0_good           	  : std_logic                    := '0';
signal d1_good           	  : std_logic                    := '0';
signal d0_sent           	  : std_logic                    := '0';
signal d1_sent           	  : std_logic                    := '0';
signal send_to_usb           : std_logic                    := '0';
signal can_send           	  : std_logic                    := '1';
signal d0_usb                : std_logic_vector(7 downto 0) := (others => '0');
signal d1_usb                : std_logic_vector(7 downto 0) := (others => '0');
signal d0_cap                : std_logic_vector(7 downto 0) := (others => '0');
signal d1_cap                : std_logic_vector(7 downto 0) := (others => '0');
signal usb_send_out          : std_logic_vector(7 downto 0) := (others => '0');
signal id_out          		  : std_logic_vector(7 downto 0) := (others => '0');

begin
	out_test <= d0_good;
	usb_data <= usb_send_out;
	usb_send <= send_to_usb;
	id <= id_out;
	
	d0_monitor : process(clk, d0_ready, reset)
	 begin
		if (reset = '1') then				
			d0_bytes <= (others => '0');			
		elsif (clk = '1' and clk'event) then
			if (d0_ready = '1') then
				d0_ram(conv_integer(d0_bytes)) <= d0;
				d0_cap <= d0;
				d0_bytes <= d0_bytes + 1;
			end if;
		end if;
	 end process d0_monitor;
	 
	 monitor_usb : process(wclk, reset)
	 begin
		if (reset = '1') then				
			d0_good <= '0';	
			d1_good <= '0';
			d0_sent <= '0';	
			d1_sent <= '0';
			send_to_usb <= '0';
			d0_bytes_read <= (others => '0');
			d1_bytes_read <= (others => '0');
			usb_send_out <= (others => '0');
			id_out <= "00000000";
			can_send <= '1';
		elsif (wclk = '1' and wclk'event) then
			can_send <= '1';
			
			if (d0_bytes_read /= d0_bytes) then				
				d0_bytes_read <= d0_bytes;
				d0_usb <= d0_cap;
				d0_good <= '1';			
			end if;
			
			if (d1_bytes_read /= d1_bytes) then				
				d1_bytes_read <= d1_bytes;
				d1_usb <= d1_cap;
				d1_good <= '1';			
			end if;
			
			if (can_send = '1') then
				if(d0_good = '1') then
					d0_good <= '0';
					d0_sent <= '1';
					usb_send_out <= d0_usb;
					send_to_usb <= '1';
					id_out <= "00000001";
					can_send <= '0';				
				elsif(d1_good = '1') then
					d1_good <= '0';
					d1_sent <= '1';
					usb_send_out <= d1_usb;
					send_to_usb <= '1';
					id_out <= "00000010";
					can_send <= '0';				
				else
					send_to_usb <= '0';					
				end if;
			else
				send_to_usb <= '0';				
			end if;
		end if;
	 end process monitor_usb;
	 
end Behavioral;

