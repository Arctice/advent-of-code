import ctypes

libssl = ctypes.CDLL("libcrypto.so")

libssl.MD5.restype = ctypes.c_void_p
md5_buffer = ctypes.create_string_buffer(16)

def md5sum(ss):
    libssl.MD5(ss.encode(), len(ss), md5_buffer)
    return tuple(md5_buffer)

def hex_digits(xs):
    hex = []
    for x in xs:
        x = ord(x)
        hex.append(x >> 4)
        hex.append(x & 0xF)
    return hex

def zero_prefix_md5(word, k):
    zeroes = [0] * k
    n = 0
    while True:
        md5 = md5sum(word + str(n))[:(k + 1) // 2]
        digits = hex_digits(md5)
        if digits[:k] == zeroes:
            return n
        n += 1

word = "bgvyzdsv"
print(zero_prefix_md5(word, 5))
print(zero_prefix_md5(word, 6))
