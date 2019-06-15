import os
from pymavlink import mavutil
os.environ["MAVLINK20"] = "TRUE"

master = mavutil.mavlink_connection("127.0.0.1:7001")
master.wait_heartbeat()

master.mav.global_position_int_send(123,23,76,100,100,20,10,30,13)

master.mav.param_value_send(bytearray("Test123",'utf-8'),10.23,1,2,3)

count = 0
while True:
    msg = master.recv_msg()
    count += 1
    if count > 2:
       exit(0)
    if msg is None:
        continue
    if msg.get_type() == "GLOBAL_POSITION_INT":
        if not (msg.time_boot_ms == 123 and
           msg.lat== 23 and
           msg.lon== 76 and
           msg.alt== 100 and
           msg.relative_alt == 100 and
           msg.vx == 20 and
           msg.vy == 10 and
           msg.vz == 30 and
           msg.hdg == 13):
            exit(-1)
    if msg.get_type() == "PARAM_VALUE":
        if not (msg.param_id == "Test123" and
                abs(msg.param_value - 10.23) < 1e-3 and
                msg.param_type == 1 and
                msg.param_count == 2 and
                msg.param_index == 3):
            exit(-1)
