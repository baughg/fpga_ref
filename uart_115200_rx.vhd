-----------------------------------------------------------
--                 UART | Receiver unit
-----------------------------------------------------------
--
-- Copyright (c) 2017
--
-----------------------------------------------------------
-- Input:      clk        | System clock at 50 MHz
--             reset      | System reset
--             rx         | RX line
--
-- Output:     data_out   | Output data
--             out_valid  | Output data valid
-----------------------------------------------------------
-- uart_115200_rx.vhd
-----------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity uart_115200_rx is
    port (clk   : in std_logic;
          reset : in std_logic;
          rx    : in std_logic;

          data_out  : out std_logic_vector(7 downto 0);
          out_valid : out std_logic;
			 out_test : out std_logic);
end entity uart_115200_rx;


architecture behavioural of uart_115200_rx is
    type   rx_state_machine is (reset_state, idle, receive_data, stop_bit);
    signal current_state, next_state : rx_state_machine;
    signal data_counter              : std_logic_vector(3 downto 0) := (others => '0');
    signal ticker                    : std_logic_vector(3 downto 0) := (others => '0');
	 signal rx_hist_0                 : std_logic_vector(3 downto 0) := (others => '0');
	 signal rx_hist_1                 : std_logic_vector(3 downto 0) := (others => '0');
	 signal rx_hist_sum               : std_logic_vector(4 downto 0) := (others => '0');
	 signal cycles                    : std_logic_vector(12 downto 0) := (others => '0');
	 signal sample_tick               : std_logic_vector(12 downto 0) := "0000011011001";
    signal data_buffer               : std_logic_vector(7 downto 0);
    signal rx_filtered               : std_logic                    := '0';
	 signal rx_filtered_prev          : std_logic                    := '0';
	 signal receiving               	 : std_logic                    := '0';
	 signal sample               	 	 : std_logic                    := '0';
	 signal new_byte               	 : std_logic                    := '0';
	 signal sample_hold               : std_logic                    := '0';
    signal rx_state                  : std_logic_vector(1 downto 0) := "11";
begin
    data_out <= data_buffer;
	 out_test <= sample;
	 out_valid <= new_byte;
	 
	 
	 filter_rx : process(clk, rx, reset)
	 begin
		if (reset = '1') then	
			rx_filtered <= '0';		
			rx_hist_0 <= (others => '0');
			rx_hist_1 <= (others => '0');
			rx_hist_sum <= (others => '0');
		elsif (clk = '1' and clk'event) then
			if (rx = '0') then
				rx_hist_0 <= rx_hist_0 + 1;			
			else
				rx_hist_1 <= rx_hist_1 + 1;
			end if;
			
			if(rx_hist_sum = 9) then
				if(rx_hist_0 > rx_hist_1) then
					rx_filtered <= '0';
				else
					rx_filtered <= '1';
				end if;				
				
				rx_hist_0 <= (others => '0');
				rx_hist_1 <= (others => '0');
				rx_hist_sum <= (others => '0');
			else 
				rx_hist_sum <= rx_hist_sum + 1;
			end if;
			
		end if;
	 end process filter_rx;
	 
	 frame_detect : process(clk, rx_filtered, reset)
	 begin
		if (reset = '1') then
			receiving <= '0';
			rx_filtered_prev <= '0';
		elsif (clk = '1' and clk'event) then
			if (rx_filtered = '0' and receiving = '0' and rx_filtered_prev = '1') then
				receiving <= '1';
				cycles <= (others => '0');				
			elsif (cycles >= 3906 and receiving = '1') then
				receiving <= '0';
				cycles <= cycles;
			else
				cycles <= cycles + 1;
			end if;

			rx_filtered_prev <= rx_filtered;
		end if;
	 end process frame_detect;
	 
    -- Updates the states in the statemachine at a 115200 bps rate
    clkgen_115k2 : process(clk, reset, current_state)
    begin
        if (reset = '1') then            
            data_buffer   <= (others => '0');
				sample_tick <= "0000011011001"; -- 0xd9
				sample_hold <= '0';
				data_counter <= (others => '0');
				new_byte <= '0';
        elsif (clk = '1' and clk'event) then            
				if (receiving = '1') then
					if(cycles >= sample_tick) then
						sample_hold <= rx_filtered;
						sample_tick <= sample_tick + 434;	
						data_counter <= data_counter + 1;
					end if;
					
					if( data_counter = 9) then
						data_counter <= (others => '0');
						data_buffer(7)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '1';
					elsif (data_counter = 2) then
						data_buffer(0)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 3) then
						data_buffer(1)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 4) then
						data_buffer(2)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 5) then
						data_buffer(3)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 6) then
						data_buffer(4)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 7) then
						data_buffer(5)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					elsif (data_counter = 8) then
						data_buffer(6)  <= sample_hold;
						sample <= sample_hold;
						new_byte <= '0';
					else
						sample <= '0';
						new_byte <= '0';
					end if;
				else
					sample_tick <= "0000011011001"; -- 0xd9
					data_counter <= (others => '0');
					sample <= '0';
					new_byte <= '0';
				end if;
        end if;
    end process clkgen_115k2;

end architecture behavioural;
