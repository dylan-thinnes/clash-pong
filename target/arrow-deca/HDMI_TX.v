module HDMI_TX(
    // CLOCK
    input CLK_25MHZ,

    // HDMI-TX
    inout         HDMI_I2C_SCL,
    inout         HDMI_I2C_SDA,
    inout   [3:0] HDMI_I2S,
    inout         HDMI_LRCLK,
    inout         HDMI_MCLK,
    inout         HDMI_SCLK,
    output        HDMI_TX_CLK,
    output [23:0] HDMI_TX_D,
    output        HDMI_TX_DE,
    output        HDMI_TX_HS,
    input         HDMI_TX_INT,
    output        HDMI_TX_VS
);
    assign HDMI_TX_CLK = CLK_25MHZ;

    topEntity u_topEntity
        (.CLK_25MHZ(CLK_25MHZ),
         .RESET(1'b0),
         .BTN_UP(1'b0),
         .BTN_DOWN(1'b0),
         .VGA_HSYNC(HDMI_TX_HS),
         .VGA_VSYNC(HDMI_TX_VS),
         .VGA_RED(HDMI_TX_D[23:16]),
         .VGA_GREEN(HDMI_TX_D[15:8]),
         .VGA_BLUE(HDMI_TX_D[7:0])
        );
endmodule
