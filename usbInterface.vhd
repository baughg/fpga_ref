----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    11:28:13 03/19/2009 
-- Design Name: 
-- Module Name:    usbInterface - Behavioral 
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

library work;
use work.all;

---- Uncomment the following library declaration if instantiating
---- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity usbInterface is
    Port ( usb_clock : in  STD_LOGIC;	--u-ifclk
           dataPort : inout  STD_LOGIC_VECTOR (7 downto 0);	--u-fd
			  id : in  STD_LOGIC_VECTOR (7 downto 0);
			  usb_data : in  STD_LOGIC_VECTOR (7 downto 0);
			  new_data : in  STD_LOGIC;
			  reset : in  STD_LOGIC;	--uflaga
           addressStrobePin : in  STD_LOGIC;	--uflaga
           dataStrobePin : in  STD_LOGIC;	--uflagb
           fpga2UsbPin : in  STD_LOGIC;		--uflagc
           waitPin : out  STD_LOGIC;	--u-slrd
			  out_test_a : out  STD_LOGIC;	--u-slrd
			  out_test_b : out  STD_LOGIC;	--u-slrd
			  debugOutputs : out std_logic_vector(12 downto 0)			  
         );  
end usbInterface;


architecture Behavioral of usbInterface is   
	signal dataIn : std_logic_vector(7 downto 0);	--input data register
	signal dataOut : std_logic_vector(7 downto 0);	--output data register
   signal busOut : std_logic_vector(7 downto 0);   --data bus output
	signal buffer_index_a : std_logic_vector(6 downto 0) := (others => '0');
	signal buffer_index_read : std_logic_vector(6 downto 0) := (others => '0');
	signal buffer_index_b : std_logic_vector(6 downto 0) := "0000001";
	signal cap_reg_addr : std_logic_vector(7 downto 0);
	signal read_reg_addr : std_logic_vector(7 downto 0);
	signal usbAddressRegister : std_logic_vector(7 downto 0);	--address register for usb
   signal total_captured : std_logic_vector(15 downto 0) := (others => '0');		
	type ram_type is array(0 to 129) of std_logic_vector(7 downto 0);
   signal captureRam : ram_type := (others => (others => '0'));
   type state_type is (s0_Ready, s1_Fpga2Usb_Data, s1_Fpga2Usb_Address, s1_Usb2Fpga_Data, s1_Usb2Fpga_Address,
                       s2_Fpga2Usb_Data, s2_Fpga2Usb_Address, s2_Usb2Fpga_Data, s2_Usb2Fpga_Address);
   
   signal currentState : state_type := s0_Ready;
   signal nextState : state_type := s0_Ready;
   
   attribute fsm_encoding : string;
   attribute fsm_encoding of currentState : signal is "auto";
	--attribute fsm_encoding of nextState : signal is "one-hot";
	attribute safe_implementation: string;
	attribute safe_implementation of currentState : signal is "yes";
	
	signal nextAddressDataMux : std_logic := '0';	--0 = address, 1 = data
	signal nextWaitOutput : std_logic := '0';
	signal nextOutputEnable : std_logic := '0';
	signal addressDataMux : std_logic := '0';	--0 = address, 1 = data
	signal waitOutput : std_logic := '0';
	signal outputEnable : std_logic := '0';
	signal ramWriteEnable : std_logic := '0'; --write enable for ram
	signal addressUpdateEnable : std_logic := '0';
	signal ramReadEnable : std_logic := '0'; --read enable for ram
	signal got_data : std_logic := '0';
	signal got_data_toggle : std_logic := '0';
	signal read_data_toggle : std_logic := '0';
	--signal char3, char2, char1, char0 : std_logic_vector(3 downto 0);
	
	
   
begin
	
	debugOutputs <= dataPort & addressStrobePin & dataStrobePin & fpga2UsbPin & waitOutput & usb_clock;
	
	--control input/output direction based on control pins
	dataPort <= busOut when outputEnable = '1' else "ZZZZZZZZ";
	busOut <= usbAddressRegister when addressDataMux = '0' else dataOut;
	out_test_a <= read_data_toggle;
	out_test_b <= got_data_toggle;
	--get input
	dataIn <= dataPort;
	waitPin <= waitOutput;
	
	MemoryProcess : process(usb_clock)   --ram control process
	begin
		if(rising_edge(usb_clock)) then
			if(new_data = '1') then		
				--cap_reg_addr <= id - 1;
				got_data <= '1';
				got_data_toggle <= not got_data_toggle;
				--captureRamNew(conv_integer(cap_reg_addr)) <= usb_data;  --write on rising edge and write enable	
				--captureRamNew(conv_integer(cap_reg_addr+2)) <= id;
				cap_reg_addr(6 downto 0) <= buffer_index_b;				
				captureRam(128) <= cap_reg_addr;
				captureRam(conv_integer(buffer_index_b)) <= usb_data;
				captureRam(conv_integer(buffer_index_a)) <= id;
				buffer_index_a <= buffer_index_a + 2;
				buffer_index_b <= buffer_index_b + 2;
				total_captured <= total_captured + 1;				
			end if;
									
			
			
			--captureRam(257) <= buffer_index_a(15 downto 8);
				
			if(ramWriteEnable = '1') then				
				captureRam(conv_integer(usbAddressRegister)) <= dataIn;  --write on rising edge and write enable
				-- testRam(conv_integer(usbAddressRegister)) <= "10100011";
			end if;
			
			if(ramReadEnable = '1') then
				buffer_index_read <= buffer_index_b;
				read_reg_addr(6 downto 0) <= buffer_index_read;
				captureRam(129) <= read_reg_addr;
				
				dataOut <= captureRam(conv_integer(usbAddressRegister)); --read on rising edge
				
				if(new_data = '0' and usbAddressRegister(0) = '0') then
					-- status register					
					captureRam(conv_integer(usbAddressRegister)) <= "00000000";
					read_data_toggle <= not read_data_toggle;											
				end if;				
			end if;		
	end if;
	end process;
	
		
	
	
	AddressProcess : process(usb_clock)
	begin
		if(rising_edge(usb_clock)) then
			if(addressUpdateEnable = '1') then	--store address
				usbAddressRegister <= dataIn;
			elsif(ramWriteEnable = '1' or ramReadEnable = '1') then
				usbAddressRegister <= usbAddressRegister + 1;
			else
				usbAddressRegister <= usbAddressRegister;
			end if;
		end if;
	end process;

	UpdateProcess : process(usb_clock)	--move the state machine along
	begin
		if(rising_edge(usb_clock)) then
			currentState <= nextState;
			outputEnable <= nextOutputEnable;
			waitOutput <= nextWaitOutput;
			addressDataMux <= nextAddressDataMux;
		end if;
	end process;
	
	NextStateProcess : process(currentState, addressStrobePin, dataStrobePin, fpga2UsbPin)
	begin
		nextState <= s0_Ready;	--default to ready state
		
		--from ready state to first level states
		if(currentState = s0_Ready) then
			if(addressStrobePin = '0' and fpga2UsbPin = '0') then
				nextState <= s1_Usb2Fpga_Address;
			elsif(addressStrobePin = '0' and fpga2UsbPin = '1') then
				nextState <= s1_Fpga2Usb_Address;
			elsif(dataStrobePin = '0' and fpga2UsbPin = '0') then
				nextState <= s1_Usb2Fpga_Data;
			elsif(dataStrobePin = '0' and fpga2UsbPin = '1') then
				nextState <= s1_Fpga2Usb_Data;
			end if;
			
		--first level to second level states
		elsif(currentState = s1_Fpga2Usb_Address) then
			nextState <= s2_Fpga2Usb_Address;
		elsif(currentState = s1_Usb2Fpga_Address) then
			nextState <= s2_Usb2Fpga_Address;
		elsif(currentState = s1_Fpga2Usb_Data) then
			nextState <= s2_Fpga2Usb_Data;
		elsif(currentState = s1_Usb2Fpga_Data) then
			nextState <= s2_Usb2Fpga_Data;
		
		--second level wait loop
		elsif(currentState = s2_Fpga2Usb_Address and addressStrobePin = '0') then
			nextState <= s2_Fpga2Usb_Address;
		elsif(currentState = s2_Usb2Fpga_Address and addressStrobePin = '0') then
			nextState <= s2_Usb2Fpga_Address;
		elsif(currentState = s2_Fpga2Usb_Data and dataStrobePin = '0') then
			nextState <= s2_Fpga2Usb_Data;
		elsif(currentState = s2_Usb2Fpga_Data and dataStrobePin = '0') then
			nextState <= s2_Usb2Fpga_Data;
		end if;
	end process;
	
   
   NextOutputProcess : process(nextState)   --determine state output
   begin
		nextOutputEnable <= '0';	--assign default outputs
		nextWaitOutput <= '1';
		nextAddressDataMux <= '0';
		ramWriteEnable <= '0';
		addressUpdateEnable <= '0';
		ramReadEnable <= '0';
      
		if(nextState = s1_Fpga2Usb_Address or nextState = s1_Fpga2Usb_Data or
		   nextState = s2_Fpga2Usb_Address or nextState = s2_Fpga2Usb_Data) then
			nextOutputEnable <= '1';
		end if;
		if(nextState = s0_Ready) then
			nextWaitOutput <= '0';
		end if;
		if(nextState = s1_Fpga2Usb_Data or nextState = s2_Fpga2Usb_Data or
		   nextState = s1_Usb2Fpga_Data or nextState = s2_Usb2Fpga_Data) then
			nextAddressDataMux <= '1';
		end if;
		if(nextState = s1_Usb2Fpga_Data) then
			ramWriteEnable <= '1';
		end if;
		if(nextState = s1_Usb2Fpga_Address) then
			addressUpdateEnable <= '1';
		end if;
		if(nextState = s1_Fpga2Usb_Data) then
			ramReadEnable <= '1';
		end if;
   end process;   
end Behavioral;

