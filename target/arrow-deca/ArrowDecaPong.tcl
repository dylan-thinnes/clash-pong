project_open ArrowDecaPong.qpf

set_global_assignment -name PROJECT_OUTPUT_DIRECTORY output_dir

package require ::quartus::flow
execute_flow -compile
