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
			  dataToUsb : in  STD_LOGIC_VECTOR (7 downto 0);	--u-fd
			  usbDataWritten : out STD_LOGIC;
			  newUsbData : in STD_LOGIC;
           addressStrobePin : in  STD_LOGIC;	--uflaga
           dataStrobePin : in  STD_LOGIC;	--uflagb
           fpga2UsbPin : in  STD_LOGIC;		--uflagc
           waitPin : out  STD_LOGIC;	--u-slrd	
			  startCapture : out  STD_LOGIC;	--u-slrd	
			  debugOutputs : out std_logic_vector(12 downto 0);
			  matchWord : out std_logic_vector(15 downto 0);
			  MOSI : out std_logic_vector(15 downto 0);
			  CmdCtrl : out std_logic_vector(15 downto 0)			  
         );  
end usbInterface;


architecture Behavioral of usbInterface is
	
   
	signal dataIn : std_logic_vector(7 downto 0);	--input data register
	signal dataOut : std_logic_vector(7 downto 0);	--output data register
   signal busOut : std_logic_vector(7 downto 0);   --data bus output
	signal usbAddressRegister : std_logic_vector(7 downto 0);	--address register for usb
   
	type ram_type is array(0 to 255) of std_logic_vector(7 downto 0);
   signal testRam : ram_type := (others => (others => '0'));   --setup 256 bytes of ram
   signal usbOutRam : ram_type := (others => (others => '0'));   --setup 256 bytes of ram
	
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
	signal usbDataWrittenFlag : std_logic := '0';
	signal sentData : std_logic := '0';
	signal usbDataStreamStarted : std_logic := '0';
	--signal char3, char2, char1, char0 : std_logic_vector(3 downto 0);
	
	
   signal usbDataOut : std_logic_vector(7 downto 0);   --data bus output
	signal usbDataOutHold : std_logic_vector(7 downto 0);   --data bus output
	signal seqOut : std_logic_vector(7 downto 0);   --test usb sequencing
	signal seqOrData : std_logic := '0';
	signal dataEquSeq : std_logic := '0';
	signal incSeq : std_logic := '1';
	signal newCapture : std_logic := '0';
	signal ackn : std_logic := '0';
	signal usbRamAddr : std_logic_vector(7 downto 0);	--address register for usb
	signal usbRamAddrSent : std_logic_vector(7 downto 0);	--address register for usb
	signal lastDataToUSB : std_logic_vector(7 downto 0);	
	signal iMatchWord : std_logic_vector(15 downto 0);
	signal iMOSI : std_logic_vector(15 downto 0);
	signal iCmdCtrl : std_logic_vector(15 downto 0);
begin
	matchWord <= iMatchWord;
	MOSI <= iMOSI;
	CmdCtrl <= iCmdCtrl;
	debugOutputs <= dataPort & addressStrobePin & dataStrobePin & fpga2UsbPin & waitOutput & usb_clock;
	
	--control input/output direction based on control pins
	dataPort <= busOut when outputEnable = '1' else "ZZZZZZZZ";
	busOut <= usbAddressRegister when addressDataMux = '0' else dataOut;
	
	--get input
	dataIn <= dataPort;
	waitPin <= waitOutput;
	usbDataWritten <= usbDataWrittenFlag;
	
	
	
process (usb_clock,testRam)
begin
if(rising_edge(usb_clock)) then
	if(ackn = '1') then
		iMatchWord(7 downto 0) <= testRam(1);
		iMatchWord(15 downto 8) <= testRam(2);
		iCmdCtrl(7 downto 0) <= testRam(5);
		iCmdCtrl(15 downto 8) <= testRam(6);
		iMOSI(7 downto 0) <= testRam(3);
		iMOSI(15 downto 8) <= testRam(4);
	end if;
end if;
end process;
	--NewUsbDataProcess : process(newUsbData,usb_clock)
	--begin
	--if(rising_edge(usb_clock)) then
		--if(newUsbData = '1') then
			--usbDataWrittenFlag <= '0';
			--usbDataStreamStarted <= '1';
			--usbDataOut <= dataToUsb;
		--else
		--	usbDataWrittenFlag <= '1';
		--else
			--usbDataStreamStarted <= '0';
		--end if;
	--end if;
	--end process;
	
	MemoryProcess : process(usb_clock, newUsbData)   --ram control process
	begin
		if(rising_edge(usb_clock)) then
			usbDataWrittenFlag <= '0';
			
			if(ramWriteEnable = '1') then
				testRam(conv_integer(usbAddressRegister)) <= dataIn;  --write on rising edge and write enable
				
				if(usbAddressRegister = "00000111") then
					ackn <= '1';
				else
					ackn <= '0';
				end if;
					
				if(dataIn = "11111110") then
					seqOut <= "10000000";
					seqOrData <= '0';
					newCapture <= '1';
				end if;
				
				
			end if;
			
			if(ramReadEnable = '1') then
				if(newUsbData = '1') then
					newCapture <= '0';
					--usbOutRam(conv_integer(usbRamAddr)) <= "11111111";
					usbOutRam(conv_integer(usbRamAddr)) <= dataToUsb;
					usbRamAddr <= usbRamAddr + 1;
					--usbDataOut <= dataToUsb;
					dataOut <= seqOut;										
					
					
					if(dataEquSeq = '0') then
						seqOrData <= '1';
					else
						seqOrData <= '0';
					end if;
					
					usbDataWrittenFlag <= '1';
				end if;
				
				
				if(seqOrData = '1') then		
					if(usbRamAddr /= usbRamAddrSent) then
						dataOut <= usbOutRam(conv_integer(usbRamAddrSent));
						lastDataToUSB <= usbOutRam(conv_integer(usbRamAddrSent));
						
						if(usbOutRam(conv_integer(usbRamAddrSent)) < seqOut) then
							dataEquSeq <= '0';							
						elsif(usbOutRam(conv_integer(usbRamAddrSent)) > seqOut) then
							dataEquSeq <= '0';							
						else
							dataEquSeq <= '1';							
						end if;
						--dataOut <= "11111111";
						seqOut <= seqOut + 1;
						usbRamAddrSent <= usbRamAddrSent + 1;
						seqOrData <= '0';
					else
						dataOut <= seqOut;
					end if;
				elsif(seqOrData = '0') then
					if(dataEquSeq = '1') then
						
						dataOut <= lastDataToUSB xor "10101010";
						seqOut <= seqOut + 1;
						dataEquSeq <= '0';
					else
						dataOut <= seqOut;						
					end if;
					if(usbRamAddr /= usbRamAddrSent) then
						seqOrData <= '1';
					else
						seqOrData <= '0';
					end if;					
				end if;
				--if(usbDataStreamStarted = '1') then
				----dataOut <= testRam(conv_integer(usbAddressRegister)); --read on rising edge
					----dataOut <= usbDataOut; --read on rising edge
										
					--dataOut <= usbDataOut;
					--seqOut <= seqOut + 1;	
					--usbDataWrittenFlag <= '1';
				--else
					--dataOut <= usbDataOut;
					--usbDataWrittenFlag <= '0';
				--end if;				
				
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

