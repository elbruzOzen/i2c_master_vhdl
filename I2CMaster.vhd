-- **********************************************************************************************************************
-- Master module for I2C and TWI(Two-Wire Interface) systems
-- Slave clock stretching is supported (slave can strecth sck before write acknowledgement)
-- No support for multi-master systems
-- RW:0->Write RW:1->Read (In 8 bit adressing, rw pin is not connected, addr(0) works as rw)
-- Supports 7, 8, 10-Bit adressing modes. Enter them as generic. (addr_mode) (7-8-10 works well. You can enter other values
-- but your design will not be synthesized)
-- * Enable is latched when busy = 0, if enable is high, busy becomes high
-- * If another high enable is latched after operation (with same address and r/w), read/write continues
-- after ack. 
-- * If high enable is latched after operation (with different address or r/w), operation continues 
-- starting with new start condition and command byte.
-- * If low enable is latched after operation, module stops working and waits for new enable
-- 
-- HOW TO USE 
-- Apply required input signals, make enable high, wait until busy is high, for 1 byte write/read make enable low.
-- For sequential read/write, change inputs after busy is high, wait until busy becomes low and high again, then change 
-- inputs again, at the end, instead of changing inputs, make enable low.
-- **********************************************************************************************************************

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity I2CMaster is
	 -- Module generics
	 Generic(
    input_clk_freq : INTEGER := 100000000;
    bus_clk_freq   : INTEGER := 400000;
	 addr_mode		 : INTEGER := 7);  
	 -- Ports
    Port ( parallel_in : in  STD_LOGIC_VECTOR(7 downto 0);
			  address : in  STD_LOGIC_VECTOR(addr_mode - 1 downto 0);
           enable : in  STD_LOGIC;
           rw : in  STD_LOGIC;
			  reset : in STD_LOGIC;
			  clk : in STD_LOGIC;
			  parallel_out : out STD_LOGIC_VECTOR(7 downto 0);
			  err : out STD_LOGIC;
			  busy : out STD_LOGIC;
			  sda : inout  STD_LOGIC;
           scl : inout  STD_LOGIC;
			  read_no_ack : in STD_LOGIC);
end I2CMaster;

architecture Behavioral of I2CMaster is
	
	-- Type declarations
	type machine_state is (stop, ready, start, command_1 , command_2, ack_ctrl_1, ack_ctrl_2, ack_ctrl_3 , ack_answer, data_send, data_recv);
	
	-- Constants
	constant COUNTER_SIZE : integer := (input_clk_freq / bus_clk_freq) / 4; 
	
	-- Enable counter
	signal enable_counter : integer range 1 to (COUNTER_SIZE) := 1; -- Counter used for creating op. enable signal
	
	-- Input signals, inputs are sampled in every clock rising edge
	signal parallel_in_signal : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU";
	signal address_signal : STD_LOGIC_VECTOR(addr_mode-1 downto 0) := (others => 'U');
	signal enable_signal : STD_LOGIC := '0';
	signal rw_signal : STD_LOGIC := '0';
	signal reset_signal : STD_LOGIC := '0';
	signal sda_input_signal : STD_LOGIC := '0';
	signal scl_input_signal : STD_LOGIC := '0';
	signal read_no_ack_signal : STD_LOGIC	:= '0';
	
	-- Output signals
	signal parallel_out_signal : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU";
	signal error_signal : STD_LOGIC := 'U';
	signal busy_signal : STD_LOGIC := 'U';
	signal sda_signal : STD_LOGIC := 'U';
	signal scl_signal : STD_LOGIC := 'U';


	-- Internal signals
	signal operation : STD_LOGIC := '0';													-- 0:Write 1:Read
	signal state : machine_state := ready; 						
	signal bit_count : integer range 0 to 8 := 8;										-- Keeps number of bits will be send
	signal substate : integer range 0 to 4 := 0;											
	signal data_buffer : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU";				-- Keeps the data will be send
	signal rec_buffer : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU";				-- Keeps received data
	signal cmd_buffer : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU";				-- Keeps adress + r/w bit
	signal cmd_buffer_2 : STD_LOGIC_VECTOR(7 downto 0) := "UUUUUUUU"; 			-- Keeps remaining of adress for 10-bit
	signal operation_enable : STD_LOGIC := '0'; 											-- Clock enable
	
begin

	process(clk)
	begin
	
		if rising_edge(clk) then
		
			-- Sample ports
			parallel_in_signal <= parallel_in;
			address_signal <= address;
			enable_signal <= enable;
			rw_signal <= rw;
			reset_signal <= reset;
			
			-- Bi-Dir Input
			sda_input_signal <= sda;
			scl_input_signal <= scl;
			
			-- Sync reset for FSM
			if reset_signal = '1' then
				state <= stop;
				busy <= '1';
			end if;
			
			-- Enable signal for setting communication speed
			if enable_counter = COUNTER_SIZE then
				enable_counter <= 1;
				operation_enable <= '1';
			else
				enable_counter <= enable_counter + 1;
				operation_enable <= '0';
			end if;
			
			if operation_enable = '1' then
			
				case state is
				
					-- STOP : Sends stop condition to serial lines --
					-- Assuming both of them are pulled high by master, first SCL, then SDA released
					when stop =>
						  if substate = 0 then
								scl_signal <= '0';
								substate <= 1;
						  elsif substate = 1 then
								sda_signal <= '0';
								substate <= 2;
						  elsif substate = 2 then
								scl_signal <= '1';
								substate <= 3;
						  elsif substate = 3 then
								sda_signal <= '1';
								substate <= 0;
								state <= ready;
						  end if;
					  
					-- READY : Waits until enable is high, if enable is high jumps into start state--
					when ready =>
						  if enable_signal = '1' then										-- New transmission will start
								data_buffer <= parallel_in_signal;						-- Sample user input into buffers
								if addr_mode = 7 then
									cmd_buffer <= address_signal & rw_signal;
									operation <= rw_signal;
								elsif addr_mode = 8 then
									cmd_buffer <= address_signal;
									operation <= address_signal(0);
								else
									cmd_buffer <= "11110" & address_signal(9 downto 8) & rw_signal;
									cmd_buffer_2 <= address_signal(7 downto 0);		-- Remaining part of address
									operation <= rw_signal;
								end if;
								read_no_ack_signal <= read_no_ack;
								state <= start;
						  else																	-- Wait for enable
								sda_signal <= '1';
								scl_signal <= '1';
								busy_signal <= '0';
						  end if;
						  
					-- START : Puts start condition into the serial line --
					when start =>
						  if substate = 0 then
						  		scl_signal <= '0';
							   error_signal <= '0';											-- Clear previous errors
							   busy_signal <= '1';
							   substate <= 1;
						  elsif substate = 1 then
							  	sda_signal <= '1';											-- Release SDA
								substate <= 2;
						  elsif substate = 2 then
								scl_signal <= '1';											-- Pull down SCL	
								substate <= 3;
						  elsif substate = 3 then
								sda_signal <= '0';
								substate <= 4;
						  elsif substate = 4 then
								scl_signal <= '0';
								state <= command_1;
								substate <= 0;
						  end if;
					
					-- COMMAND: Send command byte consisted of adress(7 downto 1) + r/w bit -- 
					when command_1 =>
						  if bit_count = 0 then															-- If bits are finished
							  bit_count <= 8;																-- Clear bit_count, jump next state
							  state <= ack_ctrl_1;
							  sda_signal <= '1';
						  else
							  if substate = 0 then
									sda_signal <= cmd_buffer(7);										-- Put data onto line
									cmd_buffer <= cmd_buffer(6 downto 0) & cmd_buffer(7);		-- Shift buffer for next bit
									substate <= 1;
							  elsif substate = 1 then													-- Then serve by ticking SCL					
									scl_signal <= '1';							
									substate <= 2;
							  elsif substate = 2 then																									
									substate <= 3;
							  elsif substate = 3 then
									scl_signal <= '0';
									substate <= 0;
									bit_count <= bit_count - 1;
							  end if;
							end if;
					
					-- COMMAND 2: Send extra bytes of 10- bit addressing
					when command_2 =>
						  if bit_count = 0 then															-- If bits are finished
							  bit_count <= 8;																-- Clear bit_count, jump next state
							  state <= ack_ctrl_3;
							  sda_signal <= '1';
						  else
							  if substate = 0 then
									sda_signal <= cmd_buffer_2(7);										-- Put data onto line
									cmd_buffer_2 <= cmd_buffer_2(6 downto 0) & cmd_buffer_2(7);		-- Shift buffer for next bit
									substate <= 1;
							  elsif substate = 1 then													-- Then serve by ticking SCL					
									scl_signal <= '1';							
									substate <= 2;
							  elsif substate = 2 then																									
									substate <= 3;
							  elsif substate = 3 then
									scl_signal <= '0';
									substate <= 0;
									bit_count <= bit_count - 1;
							  end if;
							end if;
					
					
					 -- ACK_CTRL_1 : Check if command byte recognised by any slave  --
					 -- If there is no answer, error flag is raised but operation continiues normally
					 when ack_ctrl_1 =>
							  if substate = 0 then
									scl_signal <= '1';											-- Release SCL for sampling SDA
									substate <= 1;
							  elsif substate = 1 then											-- Clock stretching
									if scl_input_signal /= '0' then
										substate <= 2;
									end if;
							  elsif substate = 2 then
									if sda_input_signal /= '0' then				-- Ack is not received
										error_signal <= '1';
									end if;
									substate <= 3;
							  elsif substate = 3 then
									scl_signal <= '0';
									substate <= 4;
							  elsif substate = 4 then
									if addr_mode = 10 then
										state <= command_2;
									else
										if operation = '0' then								-- Determine operation using sampled R/W
											state <= data_send;
										else
											state <= data_recv;
											rec_buffer <= "00000000";						-- Clear receive buffer before operation
										end if;
									end if;
									substate <= 0;
							  end if;
						
						-- ACK_CTRL_3 :Acknowledgement after second command byte (for 10-bit addressing mode)
						when ack_ctrl_3 =>
							  if substate = 0 then
									scl_signal <= '1';											-- Release SCL for sampling SDA
									substate <= 1;
							  elsif substate = 1 then										-- Clock stretching
									if scl_input_signal /= '0' then
										substate <= 2;
									end if;
							  elsif substate = 2 then
									if sda_input_signal /= '0' then				-- Ack is not received
										error_signal <= '1';
									end if;
									substate <= 3;
							  elsif substate = 3 then
									scl_signal <= '0';
									substate <= 4;
							  elsif substate = 4 then
									if operation = '0' then								-- Determine operation using sampled R/W
										state <= data_send;
									else
										state <= data_recv;
										rec_buffer <= "00000000";						-- Clear receive buffer before operation
									end if;
									substate <= 0;
							  end if;

					 -- ACK_CTRL_2 : After data transmit check if there is any slave that recognized data --
					 when ack_ctrl_2 =>
							  if substate = 0 then
									scl_signal <= '1';								-- Release line for sampling
									substate <= 1;
							  elsif substate = 1 then								-- Clock Stretching
									if scl_input_signal /= '0' then
										substate <= 2;
									end if;
							  elsif substate = 2 then
									if sda_input_signal /= '0' then				-- Ack is not received
										error_signal <= '1';
									end if;
									substate <= 3;
							  elsif substate = 3 then
									scl_signal <= '0';
									substate <= 4;
							  elsif substate = 4 then
									if enable_signal = '1' then						-- If enable is high continue transmission
										if addr_mode = 7 then
											if operation = rw_signal and cmd_buffer(7 downto 1) = address_signal then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;														-- Sample new input
												busy_signal <= '0';
												state <= data_send;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										elsif addr_mode = 8 then
											if cmd_buffer = address_signal then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;		-- Sample new input
												busy_signal <= '0';
												state <= data_send;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										else
											if cmd_buffer = "11110" & address_signal(9 downto 8) & rw_signal and cmd_buffer_2 = address_signal(7 downto 0) then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;														-- Sample new input
												busy_signal <= '0';
												state <= data_send;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										end if;
									else
										state <= stop;					-- If enable is low, put stop condition
									end if;
									substate <= 0;
									--sda_signal <= '0';
							  end if;
					
					 -- ACK_ANSWER : After receiving data, pull down SDA to indicate acknowledgement --
					 when ack_answer =>
							  if substate = 0 then
									scl_signal <= '1';					-- Serve SDA by releasing SCL
									substate <= 1;
							  elsif substate = 1 then
									substate <= 2;
							  elsif substate = 2 then
									scl_signal <= '0';
									substate <= 3;
							  elsif substate = 3 then
									if enable_signal = '1' then
										if addr_mode = 7 then
											if operation = rw_signal and cmd_buffer(7 downto 1) = address_signal then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;														-- Sample new input
												busy_signal <= '0';
												state <= data_recv;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										elsif addr_mode = 8 then
											if cmd_buffer = address_signal then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;		-- Sample new input
												busy_signal <= '0';
												state <= data_recv;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										else
											if cmd_buffer = "11110" & address_signal(9 downto 8) & rw_signal and cmd_buffer_2 = address_signal(7 downto 0) then     -- Check if operation and adress is same
												data_buffer <= parallel_in_signal;														-- Sample new input
												busy_signal <= '0';
												state <= data_recv;
											else
												state <= ready;			-- If operation or adress is changed, re-start by sending start condition 					
											end if;
										end if;
									else
										state <= stop;			-- If enable is low, put stop condition
									end if;
									substate <= 0;
							  end if;
					

					 -- DATA_SEND: Transmit given byte on serial data line --
					 when data_send =>
							  if bit_count = 0 then																-- If all bits were sent
								  bit_count <= 8;																	-- Clear bit_count and jump to next state
								  state <= ack_ctrl_2;															
								  busy_signal <= '0';															-- Means "Ok I sent it!"
								  sda_signal <= '1';
							  else
							  
								  busy_signal <= '1';															-- Previous byte transmission made it '0'
																														-- We need to make it '1' again since we are busy
								  if substate = 0 then
										sda_signal <= data_buffer(7);
										data_buffer <= data_buffer(6 downto 0) & data_buffer(7);		-- Put data onto line, then shift for next bit
										substate <= 1;
								  elsif substate = 1 then													   -- Serve data by releasing SCL
										scl_signal <= '1';
										substate <= 2;
								  elsif substate = 2 then													  
										substate <= 3;
								  elsif substate = 3 then
										scl_signal <= '0';														-- End of serving
										substate <= 0;
										bit_count <= bit_count - 1;
								  end if;
							  end if;
					  
					  
					  
					  -- DATA_RECV: Receive data which is sent by slave on serial data line --
					  when data_recv =>
							  if bit_count = 0 then															-- If 8 bit is received														
								  bit_count <= 8;																-- Clear bit_count
								  if read_no_ack_signal = '1' then
										state <= stop;															-- Jump to next state
								  else
										state <= ack_answer;
								  end if;
								  parallel_out_signal <= rec_buffer;									-- Put received data to output
								  busy_signal <= '0';														-- Means "Data is ready!"
								  sda_signal <= '0';															-- ACK
							  else
							  
								  busy_signal <= '1';														-- Previous byte transmission made it '0'
																													-- We need to make it 1 again since we are busy
								  if substate = 0 then
										substate <= 1;
								  elsif substate = 1 then
										scl_signal <= '1';													-- Release SCL for sampling
										substate <= 2;
								  elsif substate = 2 then
										rec_buffer(7) <= sda_input_signal;								-- Sample SDA
										substate <= 3;
								  elsif substate = 3 then
										rec_buffer <= rec_buffer(6 downto 0) & rec_buffer(7);		-- Shift register for next bit
										scl_signal <= '0';												
										bit_count <= bit_count - 1;
										substate <= 0;
								  end if;
							  end if; 
							  
				end case;
			end if;
			
			-- Bi-directional output depends on state
			-- Release line if sda_signal is high in some states, since line is already pulled up
			if (state = command_1 or state = command_2  or state  = data_send or state = ack_answer or state = start or state = stop) and sda_signal = '0' then
				sda <= '0';
			else
				sda <= 'Z';
			end if;
			
			-- Release line if scl_signal is high, since line is already pulled up
			if scl_signal = '0' then
				scl <= '0';
			else
				scl <= 'Z';
			end if;
			
			-- Output signals to port
			parallel_out <= parallel_out_signal;
			err <= error_signal;
			busy <= busy_signal;
			
		end if;
	end process;
end Behavioral;