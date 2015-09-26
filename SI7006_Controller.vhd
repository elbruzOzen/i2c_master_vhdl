--------------------------------  SI7006-A20-IM1 Controller -------------------------------------------------------------
-- Humidity and Temperature read pins are rsing edge sensitive. 
-- Each captured rising edge in -idle- state triggers a measurement.
-- Clk frequency => 100 MHz
-- SCK frequency => 400 KHz
-- 2 Operations : Temperature Read, Humidity Read. (Other functions of SI7006 is not included)
-- USAGE: Establish all connections. For each measurement apply rising edge to corresponding pin. If measurement is 
-- sucessfull ready pin will be asserted high. When another operation begin it will be asserted low 
-- until the oeration is finalized. After operation ready may not be asserted high if slave ack is not received properly.
-------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity SI7006_Controller is
    Port ( read_temp : in  STD_LOGIC;	-- Rising edge sensitive
           read_hum : in  STD_LOGIC;   -- Rising edge sensitive
           clk : in  STD_LOGIC;
           sda : inout  STD_LOGIC;
           scl : inout  STD_LOGIC;
           data : out  STD_LOGIC_VECTOR (15 downto 0); -- 16 bit temperature/humidity data
           ready : out  STD_LOGIC);
end SI7006_Controller;

architecture Behavioral of SI7006_Controller is

	COMPONENT I2CMaster
	GENERIC(
		 input_clk_freq : INTEGER;
		 bus_clk_freq   : INTEGER;
		 addr_mode		 : INTEGER);
	PORT(
		parallel_in : IN std_logic_vector(7 downto 0);
		address : IN std_logic_vector(addr_mode - 1 downto 0);
		enable : IN std_logic;
		rw : IN std_logic;
		reset : IN std_logic;
		clk : IN std_logic;
		read_no_ack : IN std_logic;    
		sda : INOUT std_logic;
		scl : INOUT std_logic;      
		parallel_out : OUT std_logic_vector(7 downto 0);
		err : OUT std_logic;
		busy : OUT std_logic
		);
	END COMPONENT;
	
	-- Constants, they are defined in datasheet of SI7006
	constant temp_cmd_hold_master : STD_LOGIC_VECTOR(7 downto 0) := "11100011";
	constant hum_cmd_hold_master  : STD_LOGIC_VECTOR(7 downto 0) := "11100101";
	
	--Input signals 
	signal read_temp_signal : STD_LOGIC := '0';
	signal read_hum_signal : STD_LOGIC := '0';
	
	--Output signals
	signal data_signal : STD_LOGIC_VECTOR(15 downto 0) := (others => '0');
	signal ready_signal : STD_LOGIC := '0';
	
	--Prev. values for edge detection
	signal read_temp_signal_prev : STD_LOGIC := '0';
	signal read_hum_signal_prev : STD_LOGIC := '0';
	
	--Internal signals, used for control and status check
	signal parallel_in_signal  : STD_LOGIC_VECTOR(7 downto 0);
	signal enable_signal 	   : STD_LOGIC := '0';
	signal rw_signal			   : STD_LOGIC := '0';
	signal parallel_out_signal : STD_LOGIC_VECTOR(7 downto 0);
	signal err_signal				: STD_LOGIC := '0';
	signal busy_signal			: STD_LOGIC := '0';
	signal read_no_ack_signal  : STD_LOGIC := '0';
	
	-- State variable for state machine
	signal state : integer range 0 to 11 := 0;
	
begin

	I2CMaster_Instance: I2CMaster 
	GENERIC MAP(
		input_clk_freq => 100000000,
		bus_clk_freq => 400000,
		addr_mode => 7
	)
	PORT MAP(
		parallel_in => parallel_in_signal,
		address => "1000000",						-- Address of device is fixed "1000000"
		enable => enable_signal,
		rw => rw_signal,
		reset => '0',									-- We dont need reset
		clk => clk,
		parallel_out => parallel_out_signal,
		err => err_signal,
		busy => busy_signal,
		sda => sda,
		scl => scl,
		read_no_ack => read_no_ack_signal
	);
	
	process(clk)
	begin	
		if rising_edge(clk) then
			
			--Sample inputs
			read_temp_signal <= read_temp;
			read_hum_signal <= read_hum;
			
			-- FSM
			case state is
				when 0 => 
					-- Wait for rising edge, load commands and write command
					if read_temp_signal = '1' and read_temp_signal_prev = '0' then
						parallel_in_signal <= temp_cmd_hold_master;
						rw_signal <= '0';						
						state <= 1;
					elsif read_hum_signal = '1' and read_hum_signal_prev = '0' then
						parallel_in_signal <= hum_cmd_hold_master;
						rw_signal <= '0'; 					
						state <= 1;
					end if;
				when 1 =>
					enable_signal <= '1';
					ready_signal <= '0';						-- Data is not ready now !
					state <= 2;
				when 2 =>
					if busy_signal = '1' then
						state <= 3;
					end if;
				when 3 =>
					rw_signal <= '1';							-- Then read first byte
					read_no_ack_signal <= '0'; 			-- First read, we will return ack
					state <= 4;
				when 4 =>										-- Wait for input is latched
					if busy_signal = '0' then
						state <= 5;
					end if;
				when 5 =>
					if busy_signal = '1' then
						state <= 6;
					end if;
				when 6 =>
					read_no_ack_signal <= '1';										-- Second read, no ack will be returned
					state <= 7;
				when 7 =>                              						-- Wait for input is latched
					if busy_signal = '0' then
						data_signal(15 downto 8) <= parallel_out_signal; 	-- Store MSB
						state <= 8;
					end if;
				when 8 =>
					if busy_signal = '1' then
						state <= 9;
					end if;
				when 9 =>
					enable_signal <= '0';				  							-- Disable machine
					state <= 10;
				when 10 =>
					if busy_signal = '0' then
						data_signal(7 downto 0) <= parallel_out_signal;		-- Store LSB
						state <= 11;
					end if;
				when 11 =>
					data <= data_signal;												-- Assert output
					if err_signal = '0' then										-- If error does not occur in communication
						ready_signal <= '1';											-- Probably data is ok!
					else
						ready_signal <= '0';
					end if;
					state <= 0;
			end case;
			
			--Save previous values for edge detection
			read_temp_signal_prev <= read_temp_signal;
			read_hum_signal_prev <= read_hum_signal;
			
			-- Assert outputs
			ready <= ready_signal;

		end if;
	end process;
end Behavioral;