class Main extends hxd.App {
    override function init() {
        hxd.Res.initEmbed();
        var tf = new h2d.Text(hxd.res.DefaultFont.get(), s2d);
        tf.text = "Hello from Ash!";
        tf.x = 100;
        tf.y = 100;
        tf.scale(3);
    }

    override function update(dt:Float) {}

    static function main() {
        new Main();
    }
}
