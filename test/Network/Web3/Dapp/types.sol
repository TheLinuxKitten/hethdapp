pragma solidity ^0.4.15;

contract Types {

  address owner;
  mapping (address => int) public valores;

  event NewAnon1(string log, address indexed o) anonymous;
  event NewAnon2(string log, bool a) anonymous;
  event NewAnon3(address indexed o) anonymous;
  event NewAnon4(string indexed o, bool b, uint c) anonymous;
  event New(string log, address indexed o);
  event Valor(string log, address indexed receiver, int48 valor, bool chachi);
  event Enviado(string log, address indexed from, address indexed to, int16 valor, bool chachi);

  function Types() public {
    owner = msg.sender;
    NewAnon1("TypesAnon", owner);
    New("Types", owner);
  }

  function func1(address receiver, int32 v) public {
    if (msg.sender != owner) return;
    int r = v*9;
    valores[receiver] += r;
    Valor("func1 bool true", receiver, int48(r), true);
    NewAnon1("func1 uint", receiver);
    NewAnon2("func1 int", true);
    NewAnon4("func1 int", true, 345);
  }

  function func2(address receiver, int32 amount) public {
    if (valores[msg.sender] < amount) return;
    valores[msg.sender] -= amount;
    valores[receiver] += amount;
    Valor("func2 caca", receiver, amount+10, false);
    Enviado("func2 pedo", msg.sender, receiver, int16(amount), true);
    NewAnon1("func2 pis", receiver);
    NewAnon3(msg.sender);
    NewAnon4("func2 int", false, 123);
  }

  function func3(int32 x, bool y, int8 z) public view returns (bool r, string s, address t) {
    r = (x-z) > 32 || y;
    s = "pedro";
    t = msg.sender;
  }

  function func4(uint64 a, bool[3] b, int40 c, string d, address e, 
                 bytes f, bool g, bytes3[] h) public pure returns
                (uint64 ra, bool[3] rb, int40 rc, string rd, address re, 
                 bytes rf, bool rg, bytes3[] rh) {
      ra = a;
      rb = b;
      rc = c;
      rd = d;
      re = e;
      rf = f;
      rg = g;
      rh = h;
  }

  function func5(int8 i, uint8 j)
      public pure returns (int8 ri, uint8 rj, int8 rk, uint8 rl) {
      ri = i;
      rj = j;
      rk = -1;
      rl = 0xff;
  }

  function func6() public pure returns (uint) { return 123456789; }
  function func7() public pure returns (bytes16) { return 9876; }
  function func8() public pure returns (bytes32) { return 9876; }
  function func10() public pure returns (string) { return "hola"; }
  function func11() public pure returns (uint8[10]) { return [1,2,3,4,5,6,7,8,9,0]; }

  function retString(string a) public pure returns (string) { return a; }
  function retBytes(bytes a) public pure returns (bytes) { return a; }
  function retBytes16(bytes16 a) public pure returns (bytes16) { return a; }
  function retBytes32(bytes32 a) public pure returns (bytes32) { return a; }
  function retAddress(address a) public pure returns (address) { return a; }
  function retTryUint8(uint a) public pure returns (uint8) { return uint8(a); }
  function retTryInt8(int a) public pure returns (int8) { return int8(a); }
  function retUint(uint a) public pure returns (uint) { return a; }
  function retUint8(uint8 a) public pure returns (uint8) { return a; }
  function retUint16(uint16 a) public pure returns (uint16) { return a; }
  function retUint48(uint48 a) public pure returns (uint48) { return a; }
  function retInt(int a) public pure returns (int) { return a; }
  function retInt8(int8 a) public pure returns (int8) { return a; }
  function retInt16(int16 a) public pure returns (int16) { return a; }
  function retInt48(int48 a) public pure returns (int48) { return a; }
  //function retFixArrString(string[2] a) public returns (string[2]) { return a; }
  //function retFixArrString() public returns (string[2]) { return ["hola","mundo"]; }
  //function retFixArrBytes(bytes[2] a) public returns (bytes[2]) { return a; }
  //function retFixArrBytes(bytes a, bytes b) public returns (bytes[2]) { return [a,b]; }
  function retFixArrBytes16(bytes16[2] a) public pure returns (bytes16[2]) { return a; }
  function retFixArrBytes32(bytes32[2] a) public pure returns (bytes32[2]) { return a; }
  function retFixArrAddress(address[2] a) public pure returns (address[2]) { return a; }
  function retFixArrUint(uint[2] a) public pure returns (uint[2]) { return a; }
  //function retArrString(string[] a) public returns (string[]) { return a; }
  //function retArrBytes(bytes[] a) public returns (bytes[]) { return a; }
  function retArrBytes16(bytes16[] a) public pure returns (bytes16[]) { return a; }
  function retArrBytes32(bytes32[] a) public pure returns (bytes32[]) { return a; }
  function retArrAddress(address[] a) public pure returns (address[]) { return a; }
  function retArrUint(uint[] a) public pure returns (uint[]) { return a; }

  event RetUint(uint a);
  event RetBytes16(bytes16 a);
  event RetBytes32(bytes32 a);
  event RetAddress(address a);
  event RetBytes(bytes a);
  event RetString(string a);
  event RetIdxUint(uint indexed a);
  event RetIdxBytes16(bytes16 indexed a);
  event RetIdxBytes32(bytes32 indexed a);
  event RetIdxAddress(address indexed a);
  event RetFixArrUint(uint[4] a);
  event RetFixArrBytes16(bytes16[4] a);
  event RetFixArrBytes32(bytes32[4] a);
  event RetFixArrAddress(address[4] a);
  event RetArrUint(uint[] a);
  event RetArrBytes16(bytes16[] a);
  event RetArrBytes32(bytes32[] a);
  event RetArrAddress(address[] a);

  event RetUint2(uint a, uint b);
  event RetBytes162(bytes16 a, bytes16 b);
  event RetBytes322(bytes32 a, bytes32 b);
  event RetAddress2(address a, address b);
  event RetBytes2(bytes a, bytes b);
  event RetString2(string a, string b);
  event RetIdxUint2(uint indexed a, uint indexed b);
  event RetIdxBytes162(bytes16 indexed a, bytes16 indexed b);
  event RetIdxBytes322(bytes32 indexed a, bytes32 indexed b);
  event RetIdxAddress2(address indexed a, address indexed b);
  event RetFixArrUint2(uint[4] a, uint[4] b);
  event RetFixArrBytes162(bytes16[4] a, bytes16[4] b);
  event RetFixArrBytes322(bytes32[4] a, bytes32[4] b);
  event RetFixArrAddress2(address[4] a, address[4] b);
  event RetArrUint2(uint[] a, uint[] b);
  event RetArrBytes162(bytes16[] a, bytes16[] b);
  event RetArrBytes322(bytes32[] a, bytes32[] b);
  event RetArrAddress2(address[] a, address[] b);

  event RetIdxUint5(uint indexed a, uint c, uint indexed b, uint d, uint e);
  event RetIdxBytes165(bytes16 indexed a, bytes16 b, bytes16 indexed c, bytes16 d, bytes16 e);
  event RetIdxBytes325(bytes32 indexed a, bytes32 b, bytes32 indexed c, bytes32 d, bytes32 e);
  event RetIdxAddress5(address indexed a, address b, address indexed c, address d, address e);

  function retEvents() public {
    RetUint(1234567890);
    RetBytes16("bytes16");
    RetBytes32("bytes32");
    RetAddress(msg.sender);
    RetBytes("bytes");
    RetString("string");
    RetIdxUint(9876543210);
    RetIdxBytes16("bytes16 indexed");
    RetIdxBytes32("bytes32 indexed");
    RetIdxAddress(msg.sender);
  }

  function retEventFixArrUint(uint[4] a) public {
    RetFixArrUint(a);
  }

  function retEventFixArrBytes16(bytes16[4] a) public {
    RetFixArrBytes16(a);
  }

  function retEventFixArrBytes32(bytes32[4] a) public {
    RetFixArrBytes32(a);
  }

  function retEventFixArrAddress(address[4] a) public {
    RetFixArrAddress(a);
  }

  function retEventArrUint(uint[] a) public {
    RetArrUint(a);
  }

  function retEventArrBytes16(bytes16[] a) public {
    RetArrBytes16(a);
  }

  function retEventArrBytes32(bytes32[] a) public {
    RetArrBytes32(a);
  }

  function retEventArrAddress(address[] a) public {
    RetArrAddress(a);
  }

  function retEvents2() public {
    RetUint2(1234567890, 9876543210);
    RetBytes162("bytes16", "bytes16");
    RetBytes322("bytes32", "bytes32");
    RetAddress2(msg.sender, msg.sender);
    RetBytes2("bytes", "bytes");
    RetString2("string", "string");
    RetIdxUint2(9876543210, 1234567890);
    RetIdxBytes162("bytes16 indexed", "bytes16 indexed");
    RetIdxBytes322("bytes32 indexed", "bytes32 indexed");
    RetIdxAddress2(msg.sender, msg.sender);
  }

  function retEventFixArrUint2(uint[4] a, uint[4] b) public {
    RetFixArrUint2(a,b);
  }

  function retEventFixArrBytes162(bytes16[4] a, bytes16[4] b) public {
    RetFixArrBytes162(a,b);
  }

  function retEventFixArrBytes322(bytes32[4] a, bytes32[4] b) public {
    RetFixArrBytes322(a,b);
  }

  function retEventFixArrAddress2(address[4] a, address[4] b) public {
    RetFixArrAddress2(a,b);
  }

  function retEventArrUint2(uint[] a, uint[] b) public {
    RetArrUint2(a,b);
  }

  function retEventArrBytes162(bytes16[] a, bytes16[] b) public {
    RetArrBytes162(a,b);
  }

  function retEventArrBytes322(bytes32[] a, bytes32[] b) public {
    RetArrBytes322(a,b);
  }

  function retEventArrAddress2(address[] a, address[] b) public {
    RetArrAddress2(a,b);
  }

  function retEvents5() public {
    RetIdxUint5(9876543210, 1234567890, 1234567890, 1234567890, 1234567890);
    RetIdxBytes165("bytes16 indexed", "bytes16 indexed", "bytes16 indexed", "bytes16 indexed", "bytes16 indexed");
    RetIdxBytes325("bytes32 indexed", "bytes32 indexed", "bytes32 indexed", "bytes32 indexed", "bytes32 indexed");
    RetIdxAddress5(msg.sender, msg.sender, msg.sender, msg.sender, msg.sender);
  }

}

