#lang sicp
#|
Eva的说法是正确的，pair2要比pair1的精确度高。
pair1在两次不同的计算中都引入了区间值r1和r2，产生了不同精度的结果
pair2只引用了一次区间值，虽然多次使用了 one ，但是one的精确度是确定的

|#
