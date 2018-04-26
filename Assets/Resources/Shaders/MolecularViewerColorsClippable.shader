
Shader "MolecularViewerColorsClippable"
{

	//Revealed properties of the shader.
    Properties
    {
        _Metallic("Metallic", Range(0,1)) = 0.0
		_Smoothness("Smoothness", Range(0,1)) = 0.45
        _Color("Color", Color) = (1,1,1,1)
        _Emission("Emission", Color) = (1,1,1,1)
    }

    SubShader
    {

        Tags
        {
            "RenderType" = "Transparent"
            "Queue" = "Transparent+1000"
        }

        LOD 200

        CGPROGRAM

        #pragma surface surf Standard vertex:vert alpha
		#pragma target 4.0

        struct Input
        {
            float2 uv_MainTex;
            float4 vertexColor; // Vertex color stored here by vert() method
			float vertexClip0;
            float vertexClip1;
            float vertexClip2;
            float vertexClip3;
            float vertexClip4;
            float vertexClip5;
        };


		int _invertedClipping;
		int _planeClippingEnabled;
		int _sphereClippingEnabled;
		float4 _planePos0;
		float4 _planeNorm0;
        float4 _planePos1;
		float4 _planeNorm1;
        float4 _planePos2;
		float4 _planeNorm2;
        float4 _planePos3;
		float4 _planeNorm3;
        float4 _planePos4;
		float4 _planeNorm4;
        float4 _planePos5;
		float4 _planeNorm5;
		float4 _spherePos;
		float _sphereRadius;

		float checkBehindPlane(half3 planePosition, half3 planeNormal, half3 pointInWorld)
		{
			//w = vector from plane to point
			half3 w = - ( planePosition - pointInWorld );
			half res = ( planeNormal.x * w.x + 
					planeNormal.y * w.y + 
					planeNormal.z * w.z ) 
			/ sqrt( planeNormal.x * planeNormal.x +
					planeNormal.y * planeNormal.y +
					planeNormal.z * planeNormal.z );
			return res;
		}

        void vert(inout appdata_full v, out Input o)
        {
            UNITY_INITIALIZE_OUTPUT(Input,o);
            o.vertexColor = v.color; // Save the Vertex Color in the Input for the surf() method

			o.vertexClip0 = checkBehindPlane(_planePos0, _planeNorm0, mul(unity_ObjectToWorld, v.vertex).xyz);
			o.vertexClip1 = checkBehindPlane(_planePos1, _planeNorm1, mul(unity_ObjectToWorld, v.vertex).xyz);
            o.vertexClip2 = checkBehindPlane(_planePos2, _planeNorm2, mul(unity_ObjectToWorld, v.vertex).xyz);
			o.vertexClip3 = checkBehindPlane(_planePos3, _planeNorm3, mul(unity_ObjectToWorld, v.vertex).xyz);
            o.vertexClip4 = checkBehindPlane(_planePos4, _planeNorm4, mul(unity_ObjectToWorld, v.vertex).xyz);
			o.vertexClip5 = checkBehindPlane(_planePos5, _planeNorm5, mul(unity_ObjectToWorld, v.vertex).xyz);
        }

        half _Metallic;
		half _Smoothness;
        half4 _Color;
        half4 _Emission;

        void surf(Input IN, inout SurfaceOutputStandard o)
        {
            o.Albedo = _Color.rgb; // Combine normal color with the vertex color
            // Metallic and smoothness come from slider variables
            o.Metallic = _Metallic;
            o.Smoothness = _Smoothness;
            o.Alpha = _Color.a;
            o.Emission = _Emission.rgb;
			clip(IN.vertexClip0);
            clip(IN.vertexClip1);
            clip(IN.vertexClip2);
            clip(IN.vertexClip3);
            clip(IN.vertexClip4);
            clip(IN.vertexClip5);
        }

        ENDCG
    }

    FallBack "Diffuse"

}
