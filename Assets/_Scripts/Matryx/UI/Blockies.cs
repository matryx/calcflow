using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;

namespace NetBlockies
{
    public class Blockies : IDisposable
    {
        #region Disposable implementation
        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        /// <summary>
        /// Releases unmanaged and - optionally - managed resources.
        /// </summary>
        /// <param name="disposing"><c>true</c> to release both managed and unmanaged resources; <c>false</c> to release only unmanaged resources.</param>
        protected virtual void Dispose(bool disposing)
        {
            if (_disposed)
                return;

            if (disposing)
            {
                //Free other managed objects that implement IDisposable only
            }

            _randSeed = null;
            _iconPixels = null;

            _disposed = true;
        }
        #endregion

        private bool _disposed;
        private Int32[] _randSeed = new Int32[4];
        private Color[] _iconPixels;
        private int _size;

        /// <summary>
        /// Creates identicons for ethereum addresses. The standard size of identicon is 8.
        /// </summary>
        /// <param name="Seed">Ethereum address</param>
        /// <param name="Size">Size of the identicon</param>
        public Blockies(string Seed, int size = 8)
        {
            _size = size;
            CreateImageData(Seed);
        }

        /// <summary>
        /// Returns identicon bitmap with desired resolution (nearest smaller multiple of BlockiesHelper.Size)
        /// </summary>
        /// <param name="resolution">Use multiples of BlockiesHelper.Size</param>
        /// <returns></returns>
        public Bitmap GetBitmap(int resolution) => Create(resolution / _size);

        /// <summary>
        /// Returns a base64 encoded identicon jpeg with desired resolution (nearest smaller multiple of BlockiesHelper.Size)
        /// </summary>
        /// <param name="resolution">Use multiples of BlockiesHelper.Size</param>
        /// <returns></returns>
        public string GetBase64Image(int resolution)
        {
            using (var ms = new System.IO.MemoryStream())
            {
                var image = GetBitmap(resolution);
                image.Save(ms, ImageFormat.Jpeg);
                return $"data:image/png;base64, {Convert.ToBase64String(ms.ToArray())}";
            }
        }

        private void Seedrand(string seed)
        {
            char[] seedArray = seed.ToCharArray();
            for (int i = 0; i < _randSeed.Length; i++)
                _randSeed[i] = 0;
            for (int i = 0; i < seed.Length; i++)
                _randSeed[i % 4] = ((_randSeed[i % 4] << 5) - _randSeed[i % 4]) + seedArray[i];
        }

        private double Rand()
        {
            var t = _randSeed[0] ^ (_randSeed[0] << 11);

            _randSeed[0] = _randSeed[1];
            _randSeed[1] = _randSeed[2];
            _randSeed[2] = _randSeed[3];
            _randSeed[3] = (_randSeed[3] ^ (_randSeed[3] >> 19) ^ t ^ (t >> 8));
            return Convert.ToDouble(_randSeed[3]) / Convert.ToDouble((UInt32)1 << 31);
        }

        private double HueTorgb(double p, double q, double t)
        {
            if (t < 0) t += 1;
            if (t > 1) t -= 1;
            if (t < 1D / 6) return p + (q - p) * 6 * t;
            if (t < 1D / 2) return q;
            if (t < 2D / 3) return p + (q - p) * (2D / 3 - t) * 6;
            return p;
        }

        private Color HslToRgb(double h, double s, double l)
        {
            double r, g, b;
            if (s == 0)
                r = g = b = l;
            else
            {
                var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
                var p = 2 * l - q;
                r = HueTorgb(p, q, h + 1D / 3);
                g = HueTorgb(p, q, h);
                b = HueTorgb(p, q, h - 1D / 3);
            }
            return Color.FromArgb((int)Math.Round(r * 255), (int)Math.Round(g * 255), (int)Math.Round(b * 255));
        }

        private Color CreateColor()
        {
            var h = (Rand());
            var s = ((Rand() * 0.6) + 0.4);
            var l = ((Rand() + Rand() + Rand() + Rand()) * 0.25);
            return HslToRgb(h, s, l);
        }

        private void CreateImageData(string seed)
        {
            Seedrand(seed.ToLower());

            var mainColor = CreateColor();
            var bgColor = CreateColor();
            var spotColor = CreateColor();

            int width = _size;
            int height = _size;

            int mirrorWidth = width / 2;
            int dataWidth = width - mirrorWidth;
            double[] data = new double[width * height];
            for (int i = 0; i < height; i++)
            {
                double[] row = new double[dataWidth];
                for (int x = 0; x < dataWidth; x++)
                    row[x] = Math.Floor(Rand() * 2.3);

                Array.Copy(row, 0, data, i * width, dataWidth);
                Array.Copy(row.Reverse().ToArray(), 0, data, i * width + dataWidth, mirrorWidth);
            }

            _iconPixels = new Color[data.Length];
            for (int i = 0; i < data.Length; i++)
            {
                if (data[i] == 1)
                    _iconPixels[i] = mainColor;
                else if (data[i] == 0)
                    _iconPixels[i] = bgColor;
                else
                    _iconPixels[i] = spotColor;
            }
        }

        private Bitmap Create(int scale)
        {
            Bitmap pic = new Bitmap(_size * scale, _size * scale, PixelFormat.Format32bppArgb);
            for (int i = 0; i < _iconPixels.Length; i++)
            {
                int x = i % _size;
                int y = i / _size;
                for (int xx = x * scale; xx < x * scale + scale; xx++)
                    for (int yy = y * scale; yy < y * scale + scale; yy++)
                        pic.SetPixel(xx, yy, _iconPixels[i]);
            }
            return pic;
        }
    }
}