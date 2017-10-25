Shader "RS/DiffuseBump" 
{
	Properties 
	{
		_Color		("Color", 	Color) 	= (1.0,1.0,1.0,1.0)
		_MainTex	("Texture", 2D) 	= "white" {}
	  	_BumpMap 	("Bumpmap", 2D) 	= "bump" {}
	}

	SubShader 
	{
	  Tags { "RenderType" = "Opaque" }

	  CGPROGRAM
	  #pragma surface surf Lambert
	  struct Input 
	  {
	    float2 uv_MainTex;
	    float2 uv_BumpMap;
	  };
	  
	  sampler2D _MainTex;
	  sampler2D _BumpMap;
	  float4	_Color;
	  
	  void surf (Input IN, inout SurfaceOutput o) 
	  {
	    o.Albedo = tex2D (_MainTex, IN.uv_MainTex).rgb * _Color.rgb;
	    o.Normal = tex2D (_BumpMap, IN.uv_BumpMap) * 2 - 1;
	  }
	  ENDCG
	} 
	
	Fallback "Diffuse"
}