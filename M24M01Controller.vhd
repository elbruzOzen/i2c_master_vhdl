--**********************************************************************************************************
--  	This module is designed to interface with M24M01 128K Byte I2C Serial EEPROM
-- 	EEPROM address must be 1010-E2-E1-A16 for M24M01 (Note that A16 is MSB of register address)
--		Start is rising edge sensitive and does operation once. Apply a rising edge after set all other inputs.
-- Wait until busy becomes low. Error means I2C master is recognized or you entered invalid operation.
-- 	Operation "00"  Current Adress Read + Increment Adress 
-- 	Operation "01" : Random Read : Read given register address and return as parallel output.
-- Register address must be provided
--		Operation "10" : Byte Write : Write given byte to given address
-- Register address and parallel input must be provided
-- 	Operation "11" : Invalid operation returns err = '1'


  ------- EEPROM address ----------
--| 1 | 0 | 1 | 0 | E2 | E1 | A16 |
  ---------------------------------
  ------- REGISTER address --------
--| A15 | ..................|  A0 |
  ---------------------------------
  
--**********************************************************************************************************


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity M24M01Controller is
	  Port ( 
		  parallel_in : in  STD_LOGIC_VECTOR(7 downto 0);
		  register_adress : in STD_LOGIC_VECTOR(15 downto 0);		-- Register address inside EEPROM (128K Byte Adressing)
		  eeprom_adress : in STD_LOGIC_VECTOR(6 downto 0);			-- I2C Address of EEPROM (LSB is MSB of register address)
		  start : in  STD_LOGIC;											-- Rising edge sensitive
		  reset : in STD_LOGIC;												-- Reset for I2C Master
		  clk : in STD_LOGIC;
		  parallel_out : out STD_LOGIC_VECTOR(7 downto 0);
		  err : out STD_LOGIC;
		  busy : out STD_LOGIC;
		  sda : inout  STD_LOGIC;
		  scl : inout  STD_LOGIC;
		  operation : in STD_LOGIC_VECTOR(1 downto 0));				-- Operation select
end M24M01Controller;

architecture Behavioral of M24M01Controller is

	COMPONENT I2CMaster
	GENERIC(
		input_clk_freq : INTEGER;
		bus_clk_freq : INTEGER;
		addr_mode : INTEGER
	);
	PORT(
		parallel_in : IN std_logic_vector(7 downto 0);
		address : IN std_logic_vector(6 downto 0);
		enable : IN std_logic;
		rw : IN std_logic;
		reset : IN std_logic;
		clk : IN std_logic;    
		sda : INOUT std_logic;
		scl : INOUT std_logic;      
		parallel_out : OUT std_logic_vector(7 downto 0);
		err : OUT std_logic;
		busy : OUT std_logic;
		read_no_ack : IN std_logic											-- '1' => Read but shown no ack, sometimes necessary
		);
	END COMPONENT;

	--Internal signals
	signal enable_signal 		: STD_LOGIC := '0';
	signal start_signal_buffer : STD_LOGIC_VECTOR(1 downto 0) := "00";
	signal start_signal 			: STD_LOGIC := '0';
	signal data_in					: STD_LOGIC_VECTOR(7 downto 0) := "00000000";
	signal busy_signal 			: STD_LOGIC	:= '0';
	signal state					: integer range 0 to 19 := 0;
	signal read_no_ack_signal 	: STD_LOGIC := '0';
	signal operation_signal		: STD_LOGIC_VECTOR(1 downto 0) := "00";
	signal rw_signal				: STD_LOGIC := '0';
	signal error_signal_i2c		: STD_LOGIC := '0';
	signal invalid_op				: STD_LOGIC := '0';
	
begin

	Instance_I2CMaster: I2CMaster 
	GENERIC MAP(
		input_clk_freq => 100000000,
		bus_clk_freq => 1000000,				-- Maximum SCL speed supported by M24M01, 400KHz or 100Khz also works
		addr_mode => 7
	)
	PORT MAP(
		parallel_in => data_in,
		address => eeprom_adress,									
		enable => enable_signal,
		rw => rw_signal,
		reset => reset,
		clk => clk,
		parallel_out => parallel_out,
		err => error_signal_i2c,
		busy => busy_signal,
		sda => sda,
		scl => scl,
		read_no_ack => read_no_ack_signal
	);
	
	process(clk)
	begin
		if rising_edge(clk) then
			
			
			-- Detect rising edge
			start_signal_buffer(0) <= start;
			if start_signal_buffer(1) = '0' and start_signal_buffer(0) = '1' then
				start_signal <= '1';
			else
				start_signal <= '0';
			end if;
			start_signal_buffer(1) <= start_signal_buffer(0);
			
			-- FSM of EEPROMController
			case state is 
				when 0 =>										-- Wait for start signal (Rising Edge)
					if start_signal = '1' then
						operation_signal <= operation;
						read_no_ack_signal <= '0';
						invalid_op <= '0';
						state <= 1;
					end if;
				when 1 =>										-- Determine operation
					case operation_signal is
						when "00" => state <= 2;			-- Current byte read
						when "10" => state <= 5;			-- Byte Write
						when "01" => state <= 12;			-- Random Read		
						when others => state <= 19;		-- Invalid Operation
					end case;
				-- CURRENT BYTE READ --
				when 2 =>
					rw_signal <= '1';							-- Read Operation
					read_no_ack_signal <= '1';				-- M24M01 does not require ack for reading operation
					state <= 3;
				when 3 =>
					enable_signal <= '1';					-- Enable I2C Master
					state <= 4;
				when 4 =>
					if busy_signal = '1' then				-- If operation is started, we can stop sending signal
						enable_signal <= '0';
						state <= 0;
					end if;
				-- ADRESSED BYTE WRITE --
				when 5 =>
					rw_signal <= '0';
					data_in <= register_adress(15 downto 8);			-- 1. Command byte + register address(15-8) + register address(7-0) +  parallel input
					state <= 6;
				when 6 =>
					enable_signal <= '1';
					state <= 7;
				when 7 =>
					if busy_signal = '1' then
						data_in <= register_adress(7 downto 0);		-- Serve other part of register address (7-0)
						state <= 8;
					end if;
				when 8 =>
					if busy_signal = '0' then
						state <= 9;
					end if;
				when 9 =>
					if busy_signal = '1' then
						data_in <= parallel_in;								-- Serve data
						state <= 10;
					end if;
				when 10 =>
					if busy_signal = '0' then
						state <= 11;
					end if;
				when 11 =>
					if busy_signal = '1' then
						enable_signal <= '0';								-- If all data is latched, make enable low
						state <= 0;												-- Return initial state
					end if;
				-- RANDOM READ -- 
				when 12 =>
					rw_signal <= '0';
					data_in <= register_adress(15 downto 8);			-- Provide register address to read(15-8)
					state <= 13;
				when 13 =>
					enable_signal <= '1';
					state <= 14;
				when 14 =>
					if busy_signal = '1' then
						data_in <= register_adress(7 downto 0);      -- Provide register address to read(7-0)
						state <= 15;
					end if;
				when 15 =>
					if busy_signal = '0' then
						state <= 16;
					end if;
				when 16 =>
					if busy_signal = '1' then
						rw_signal <= '1';										-- Register address is fetched, send start again and read data
						read_no_ack_signal <= '1';							-- Show no ACK
						state <= 17;
					end if;
				when 17 =>
					if busy_signal = '0' then
						state <= 18;
					end if;
				when 18 =>
					if busy_signal = '1' then
						enable_signal <= '0';
						state <= 0;
					end if;
				-- INVALID OP --
				when others =>
					invalid_op <= '1';										-- Invalid operation is entered
					state <= 0;
					
			end case;
			
			busy <= busy_signal;
			err <= invalid_op or error_signal_i2c;						-- Invalid operation or I2C master can raise error flag
			
		end if;
	end process;
end Behavioral;